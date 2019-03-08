const calllike = Set([:call, :foreigncall])

function extract_inner_call!(stmt, idx, once::Bool=false)
    isa(stmt, Expr) || return nothing
    (stmt.head == :toplevel || stmt.head == :thunk) && return nothing
    once |= stmt.head ∈ calllike
    for (i, a) in enumerate(stmt.args)
        isa(a, Expr) || continue
        # Make sure we don't "damage" special syntax that requires literals
        if i == 1 && stmt.head == :foreigncall
            continue
        end
        if i == 2 && stmt.head == :call && stmt.args[1] == :cglobal
            continue
        end
        ret = extract_inner_call!(a, idx, once) # doing this first extracts innermost calls
        ret !== nothing && return ret
        iscalllike = a.head ∈ calllike
        if once && iscalllike
            stmt.args[i] = NewSSAValue(idx)
            return a
        end
    end
    return nothing
end

function replace_ssa!(stmt, ssalookup)
    isa(stmt, Expr) || return nothing
    for (i, a) in enumerate(stmt.args)
        if isa(a, SSAValue)
            stmt.args[i] = SSAValue(ssalookup[a.id])
        elseif isa(a, NewSSAValue)
            stmt.args[i] = SSAValue(a.id)
        else
            replace_ssa!(a, ssalookup)
        end
    end
    return nothing
end

function renumber_ssa!(stmts::Vector{Any}, ssalookup)
    for (i, stmt) in enumerate(stmts)
        if isa(stmt, GotoNode)
            stmts[i] = GotoNode(ssalookup[stmt.label])
        elseif isa(stmt, SSAValue)
            stmts[i] = SSAValue(ssalookup[stmt.id])
        elseif isa(stmt, NewSSAValue)
            stmts[i] = SSAValue(stmt.id)
        elseif isa(stmt, Expr)
            replace_ssa!(stmt, ssalookup)
            if (stmt.head == :gotoifnot || stmt.head == :enter) && isa(stmt.args[end], Int)
                stmt.args[end] = ssalookup[stmt.args[end]]
            end
        end
    end
    return stmts
end

# Pre-frame-construction lookup
function lookup_stmt(stmts, arg)
    if isa(arg, SSAValue)
        arg = stmts[arg.id]
    end
    if isa(arg, QuoteNode)
        arg = arg.value
    end
    return arg
end

function smallest_ref(stmts, arg, idmin)
    if isa(arg, SSAValue)
        idmin = min(idmin, arg.id)
        return smallest_ref(stmts, stmts[arg.id], idmin)
    elseif isa(arg, Expr)
        for a in arg.args
            idmin = smallest_ref(stmts, a, idmin)
        end
    end
    return idmin
end

function lookup_global_refs!(ex::Expr)
    (ex.head == :isdefined || ex.head == :thunk || ex.head == :toplevel) && return nothing
    for (i, a) in enumerate(ex.args)
        ex.head == :(=) && i == 1 && continue # Don't look up globalrefs on the LHS of an assignment (issue #98)
        if isa(a, GlobalRef)
            if isdefined(a.mod, a.name) && isconst(a.mod, a.name)
                r = getfield(a.mod, a.name)
                ex.args[i] = QuoteNode(r)
            end
        elseif isa(a, Expr)
            lookup_global_refs!(a)
        end
    end
    return nothing
end

"""
    optimize!(code::CodeInfo, mod::Module)

Perform minor optimizations on the lowered AST in `code` to reduce execution time
of the interpreter.
Currently it looks up `GlobalRef`s (for which it needs `mod` to know the scope in
which this will run) and ensures that no statement includes nested `:call` expressions
(splitting them out into multiple SSA-form statements if needed).
"""
function optimize!(code::CodeInfo, mod::Module)
    code.inferred && error("optimization of inferred code not implemented")
    # TODO: because of builtins.jl, for CodeInfos like
    #   %1 = Core.apply_type
    #   %2 = (%1)(args...)
    # it would be best to *not* resolve the GlobalRef at %1

    ## Replace GlobalRefs with QuoteNodes
    for (i, stmt) in enumerate(code.code)
        if isa(stmt, GlobalRef)
            code.code[i] = QuoteNode(getfield(stmt.mod, stmt.name))
        elseif isa(stmt, Expr)
            if stmt.head == :call && stmt.args[1] == :cglobal  # cglobal requires literals
                continue
            else
                lookup_global_refs!(stmt)
            end
        end
    end

    ## Un-nest :call expressions (so that there will be only one :call per line)
    # This will allow us to re-use args-buffers rather than having to allocate new ones each time.
    old_code, old_codelocs = code.code, code.codelocs
    code.code = new_code = eltype(old_code)[]
    code.codelocs = new_codelocs = Int32[]
    ssainc = fill(1, length(old_code))
    for (i, stmt) in enumerate(old_code)
        loc = old_codelocs[i]
        inner = extract_inner_call!(stmt, length(new_code)+1)
        while inner !== nothing
            push!(new_code, inner)
            push!(new_codelocs, loc)
            ssainc[i] += 1
            inner = extract_inner_call!(stmt, length(new_code)+1)
        end
        push!(new_code, stmt)
        push!(new_codelocs, loc)
    end
    # Fix all the SSAValues and GotoNodes
    ssalookup = cumsum(ssainc)
    renumber_ssa!(new_code, ssalookup)
    code.ssavaluetypes = length(new_code)

    # Replace :llvmcall and :foreigncall with compiled variants. See
    # https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/13#issuecomment-464880123
    methodtables = Vector{Union{Compiled,TypeMapEntry}}(undef, length(code.code))
    for (idx, stmt) in enumerate(code.code)
        if isexpr(stmt, :call)
            # Check for :llvmcall
            arg1 = stmt.args[1]
            if arg1 == :llvmcall || lookup_stmt(code.code, arg1) == Base.llvmcall
                uuid = uuid4()
                ustr = replace(string(uuid), '-'=>'_')
                methname = Symbol("llvmcall_", ustr)
                nargs = length(stmt.args)-4
                argnames = [Symbol("arg", string(i)) for i = 1:nargs]
                # Run a mini-interpreter to extract the types
                framecode = JuliaFrameCode(CompiledCalls, code; optimize=false)
                frame = prepare_locals(framecode, [])
                idxstart = idx
                for i = 2:4
                    idxstart = smallest_ref(code.code, stmt.args[i], idxstart)
                end
                frame.pc[] = JuliaProgramCounter(idxstart)
                while true
                    pc = step_expr!(Compiled(), frame)
                    convert(Int, pc) == idx && break
                    pc === nothing && error("this should never happen")
                end
                str, RetType, ArgType = @lookup(frame, stmt.args[2]), @lookup(frame, stmt.args[3]), @lookup(frame, stmt.args[4])
                def = quote
                    function $methname($(argnames...))
                        return Base.llvmcall($str, $RetType, $ArgType, $(argnames...))
                    end
                end
                f = Core.eval(CompiledCalls, def)
                stmt.args[1] = QuoteNode(f)
                deleteat!(stmt.args, 2:4)
                methodtables[idx] = Compiled()
            end
        end
    end

    return code, methodtables
end
