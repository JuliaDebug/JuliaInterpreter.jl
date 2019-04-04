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

function compute_ssa_mapping_delete_statements!(code::CodeInfo, stmts::Vector{Int})
    stmts = unique!(sort!(stmts))
    ssalookup = collect(1:length(code.codelocs))
    cnt = 1
    for i in 1:length(stmts)
        start = stmts[i] + 1
        stop = i == length(stmts) ? length(code.codelocs) : stmts[i+1]
        ssalookup[start:stop] .-= cnt
        cnt += 1
    end
    return ssalookup
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

function lookup_global_ref(a::GlobalRef)
    if isdefined(a.mod, a.name) && isconst(a.mod, a.name)
        r = getfield(a.mod, a.name)
        return QuoteNode(r)
    else
        return a
    end
end

function lookup_global_refs!(ex::Expr)
    (ex.head == :isdefined || ex.head == :thunk || ex.head == :toplevel) && return nothing
    for (i, a) in enumerate(ex.args)
        ex.head == :(=) && i == 1 && continue # Don't look up globalrefs on the LHS of an assignment (issue #98)
        if isa(a, GlobalRef)
            ex.args[i] = lookup_global_ref(a)
        elseif isa(a, Expr)
            lookup_global_refs!(a)
        end
    end
    return nothing
end

const rng = MersenneTwister()

"""
    optimize!(code::CodeInfo, mod::Module)

Perform minor optimizations on the lowered AST in `code` to reduce execution time
of the interpreter.
Currently it looks up `GlobalRef`s (for which it needs `mod` to know the scope in
which this will run) and ensures that no statement includes nested `:call` expressions
(splitting them out into multiple SSA-form statements if needed).
"""
function optimize!(code::CodeInfo, scope)
    mod = moduleof(scope)
    sparams = scope isa Method ? Symbol[sparam_syms(scope)...] : Symbol[]
    code.inferred && error("optimization of inferred code not implemented")
    # TODO: because of builtins.jl, for CodeInfos like
    #   %1 = Core.apply_type
    #   %2 = (%1)(args...)
    # it would be best to *not* resolve the GlobalRef at %1
    ## Replace GlobalRefs with QuoteNodes
    for (i, stmt) in enumerate(code.code)
        if isa(stmt, GlobalRef)
            code.code[i] = lookup_global_ref(stmt)
        elseif isa(stmt, Expr)
            if stmt.head == :call && stmt.args[1] == :cglobal  # cglobal requires literals
                continue
            else
                lookup_global_refs!(stmt)
            end
        end
    end

    # Replace :llvmcall and :foreigncall with compiled variants. See
    # https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/13#issuecomment-464880123
    foreigncalls_idx = Int[]
    delete_idxs = Int[]
    for (idx, stmt) in enumerate(code.code)
        # Foregincalls can be rhs of assignments
        if isexpr(stmt, :(=))
            stmt = stmt.args[2]
        end
        if isexpr(stmt, :call)
            # Check for :llvmcall
            arg1 = stmt.args[1]
            if (arg1 == :llvmcall || lookup_stmt(code.code, arg1) == Base.llvmcall) && isempty(sparams) && scope isa Method
                uuid = uuid1(rng)
                ustr = replace(string(uuid), '-'=>'_')
                methname = Symbol("llvmcall_", ustr)
                nargs = length(stmt.args)-4
                delete_idx = build_compiled_call!(stmt, methname, Base.llvmcall, stmt.args[2:4], code, idx, nargs, sparams)
                push!(foreigncalls_idx, idx)
                append!(delete_idxs, delete_idx)
            end
        elseif isexpr(stmt, :foreigncall) && scope isa Method
            f = lookup_stmt(code.code, stmt.args[1])
            if isa(f, Ptr)
                f = string(uuid1(rng))
            elseif isexpr(f, :call)
                length(f.args) == 3 || continue
                if !(f.args[1] === tuple || f.args[1] == :($(QuoteNode(tuple))))
                    continue
                end
                lib = f.args[3] isa String ? f.args[3] : f.args[3].value
                prefix = f.args[2] isa String ? f.args[2] : f.args[2].value
                f = Symbol(prefix, '_', lib)
            end
            # Punt on non literal ccall arguments for now
            if !(isa(f, String) || isa(f, Symbol) || isa(f, Ptr))
                continue
            end
            # TODO: Only compile one ccall per call and argument types
            uuid = uuid1(rng)
            ustr = replace(string(uuid), '-'=>'_')
            methname = Symbol("ccall", '_', f, '_', ustr)
            nargs = stmt.args[5]
            delete_idx = build_compiled_call!(stmt, methname, :ccall, stmt.args[1:3], code, idx, nargs, sparams)
            push!(foreigncalls_idx, idx)
            append!(delete_idxs, delete_idx)
        end
    end

    if !isempty(delete_idxs)
        ssalookup = compute_ssa_mapping_delete_statements!(code, delete_idxs)
        foreigncalls_idx = map(x -> ssalookup[x], foreigncalls_idx)
        deleteat!(code.codelocs, delete_idxs)
        deleteat!(code.code, delete_idxs)
        code.ssavaluetypes = length(code.code)
        renumber_ssa!(code.code, ssalookup)
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

    # Insert the foreigncall wrappers at the updated idxs
    methodtables = Vector{Union{Compiled,TypeMapEntry}}(undef, length(code.code))
    for idx in foreigncalls_idx
        methodtables[ssalookup[idx]] = Compiled()
    end

    return code, methodtables
end

function parametric_type_to_expr(t::Type)
    t isa Core.TypeofBottom && return t
    t isa UnionAll && (t = t.body)
    if t <: Vararg 
        return Expr(:(...), t.parameters[1])
    end
    return t.hasfreetypevars ? Expr(:curly, t.name.name, ((tv-> tv isa TypeVar ? tv.name : tv).(t.parameters))...) : t
end

# Handle :llvmcall & :foreigncall (issue #28)
function build_compiled_call!(stmt, methname, fcall, typargs, code, idx, nargs, sparams)
    argnames = Any[Symbol("arg", string(i)) for i = 1:nargs]
    delete_idx = Int[]
    if fcall == :ccall
        cfunc, RetType, ArgType = lookup_stmt(code.code, stmt.args[1]), stmt.args[2], stmt.args[3]
        # The result of this is useful to have next to you when reading this code:
        # f(x, y) =  ccall(:jl_value_ptr, Ptr{Cvoid}, (Float32,Any), x, y)
        # @code_lowered f(2, 3)
        args = []
        for (atype, arg) in zip(ArgType, stmt.args[6:6+nargs-1])
            if atype === Any
                push!(args, arg)
            else
                @assert arg isa SSAValue
                unsafe_convert_expr = code.code[arg.id]
                push!(delete_idx, arg.id) # delete the unsafe_convert
                cconvert_stmt = unsafe_convert_expr.args[3]
                push!(delete_idx, cconvert_stmt.id) # delete the cconvert
                cconvert_expr = code.code[cconvert_stmt.id]
                push!(args, cconvert_expr.args[3])
            end
        end
    else
        # Run a mini-interpreter to extract the types
        framecode = FrameCode(CompiledCalls, code; optimize=false)
        frame = Frame(framecode, prepare_framedata(framecode, []))
        idxstart = idx
        for i = 2:4
            idxstart = smallest_ref(code.code, stmt.args[i], idxstart)
        end
        frame.pc = idxstart
        if idxstart < idx
            while true
                pc = step_expr!(Compiled(), frame)
                pc == idx && break
                pc === nothing && error("this should never happen")
            end
        end
        cfunc, RetType, ArgType = @lookup(frame, stmt.args[2]), @lookup(frame, stmt.args[3]), @lookup(frame, stmt.args[4])
        args = stmt.args[5:end]
    end
    if isa(cfunc, Expr)
        cfunc = eval(cfunc)
    end
    if isa(cfunc, Symbol)
        cfunc = QuoteNode(cfunc)
    end
    if fcall == :ccall
        ArgType = Expr(:tuple, [parametric_type_to_expr(t) for t in ArgType]...)
    end
    if isa(RetType, SimpleVector)
        @assert length(RetType) == 1
        RetType = RetType[1]
    end
    RetType = parametric_type_to_expr(RetType)
    wrapargs = copy(argnames)
    for sparam in sparams
        push!(wrapargs, :(::Val{$sparam}))
    end
    if stmt.args[4] == :(:llvmcall)
        def = :(
            function $methname($(wrapargs...)) where {$(sparams...)}
                return $fcall($cfunc, llvmcall, $RetType, $ArgType, $(argnames...))
            end)
    elseif stmt.args[4] == :(:stdcall)
        def = :(
            function $methname($(wrapargs...)) where {$(sparams...)}
                return $fcall($cfunc, stdcall, $RetType, $ArgType, $(argnames...))
            end)
    else
        def = :(
            function $methname($(wrapargs...)) where {$(sparams...)}
                return $fcall($cfunc, $RetType, $ArgType, $(argnames...))
            end)
    end
    f = Core.eval(CompiledCalls, def)
    stmt.args[1] = QuoteNode(f)
    stmt.head = :call
    deleteat!(stmt.args, 2:length(stmt.args))
    append!(stmt.args, args)
    for i in 1:length(sparams)
        push!(stmt.args, :($Val($(Expr(:static_parameter, i)))))
    end
    return delete_idx
end

