"""
    local_variables = locals(frame::JuliaStackFrame)::Vector{Variable}

Return the local variables as a vector of `Variable`[@ref].
"""
function locals(frame::JuliaStackFrame)
    vars = Variable[]
    for (sym, idx) in frame.last_reference
        push!(vars, Variable(something(frame.locals[idx]), sym, false))
    end
    reverse!(vars)
    if frame.code.scope isa Method
        syms = sparam_syms(frame.code.scope)
        for i in 1:length(syms)
            push!(vars, Variable(frame.sparams[i], syms[i], true))
        end
    end
    return vars
end

function moduleof(x)
    if isa(x, JuliaStackFrame)
        x = x.code.scope
    end
    return _moduleof(x)
end
_moduleof(scope::Method) = scope.module
_moduleof(scope::Module) = scope
Base.nameof(frame) = isa(frame.code.scope, Method) ? frame.code.scope.name : nameof(frame.code.scope)

is_loc_meta(expr, kind) = isexpr(expr, :meta) && length(expr.args) >= 1 && expr.args[1] === kind

"""
    isglobalref(g, mod, name)

Tests whether `g` is equal to `GlobalRef(mod, name)`.
"""
isglobalref(g, mod, name) = isa(g, GlobalRef) && g.mod === mod && g.name == name

function to_function(x)
    if isa(x, GlobalRef)
        getfield(x.mod, x.name)
    else
        x
    end
end

_Typeof(x) = isa(x,Type) ? Type{x} : typeof(x)

function is_function_def(ex)
    (isexpr(ex, :(=)) && isexpr(ex.args[1], :call)) ||
    isexpr(ex,:function)
end

function is_generated(meth)
    isdefined(meth, :generator)
end

function whichtt(tt)
    m = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), tt, typemax(UInt))
    m === nothing && return nothing
    return m.func::Method
end

"""
    is_doc_expr(ex)

Test whether expression `ex` is a `@doc` expression.
"""
function is_doc_expr(ex)
    docsym = Symbol("@doc")
    if isexpr(ex, :macrocall)
        a = ex.args[1]
        isglobalref(a, Core, docsym) && return true
        isa(a, Symbol) && a == docsym && return true
        if isexpr(a, :.)
            mod, name = a.args[1], a.args[2]
            return mod === :Core && isa(name, QuoteNode) && name.value == docsym
        end
    end
    return false
end

get_source(meth) = Base.uncompressed_ast(meth)

function get_source(g::GeneratedFunctionStub)
    b = g(g.argnames...)
    b isa CodeInfo && return b
    return eval(b)
end

function copy_codeinfo(code::CodeInfo)
    @static if VERSION < v"1.1.0-DEV.762"
        newcode = ccall(:jl_new_struct_uninit, Any, (Any,), CodeInfo)::CodeInfo
        for (i, name) in enumerate(fieldnames(CodeInfo))
            if isdefined(code, name)
                val = getfield(code, name)
                ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), newcode, i-1, val===nothing || isa(val, Union{Type, Method}) ? val : copy(val))
            end
        end
        return newcode
    else
        # Inline this when support for VERSION above is dropped
        return copy(code)
    end
end

lower(mod, arg) = false ? expand(arg) : Meta.lower(mod, arg)

separate_kwargs(args...; kwargs...) = (args, kwargs.data)

"""
Determine whether we are calling a function for which the current function
is a wrapper (either because of optional arguments or becaue of keyword arguments).
"""
function iswrappercall(expr)
    isexpr(expr, :(=)) && (expr = expr.args[2])
    isexpr(expr, :call) && any(x->x==SlotNumber(1), expr.args)
end

function is_call(node)
    isexpr(node, :call) ||
    (isexpr(node, :(=)) && (isexpr(node.args[2], :call)))
end

pc_expr(frame, pc) = frame.code.code.code[pc.next_stmt]
pc_expr(frame) = pc_expr(frame, frame.pc[])

function find_used(code::CodeInfo)
    used = BitSet()
    stmts = code.code
    for stmt in stmts
        Core.Compiler.scan_ssa_use!(push!, used, stmt)
        if isexpr(stmt, :struct_type)  # this one is missed
            for a in stmt.args
                Core.Compiler.scan_ssa_use!(push!, used, a)
            end
        end
    end
    return used
end

isgotonode(node) = isa(node, GotoNode) || isexpr(node, :gotoifnot)

"""
    loc = whereis(frame, pc=frame.pc[])

Return the file and line number for `frame` at `pc`.  If this cannot be
determined, `loc == nothing`. Otherwise `loc == (filepath, line)`.

When `frame` represents top-level code,
"""
function CodeTracking.whereis(framecode::JuliaFrameCode, pc)
    codeloc = codelocation(framecode.code, convert(Int, pc))
    codeloc == 0 && return nothing
    lineinfo = framecode.code.linetable[codeloc]
    return framecode.scope isa Method ?
        whereis(lineinfo, framecode.scope) : string(lineinfo.file), lineinfo.line
end
CodeTracking.whereis(frame::JuliaStackFrame, pc=frame.pc[]) = whereis(frame.code, pc)

# Note: linenumber is now an internal method for use by `next_line!`
# If you want to know the actual line number in a file, call `whereis`.
function linenumber(framecode::JuliaFrameCode, pc)
    codeloc = codelocation(framecode.code, convert(Int, pc))
    codeloc == 0 && return nothing
    return framecode.code.linetable[codeloc].line
end
linenumber(frame::JuliaStackFrame, pc=frame.pc[]) = linenumber(frame.code, pc)

function codelocation(code::CodeInfo, idx)
    codeloc = code.codelocs[idx]
    while codeloc == 0 && code.code[idx] === nothing && idx < length(code.code)
        idx += 1
        codeloc = code.codelocs[idx]
    end
    return codeloc
end

"""
    stmtidx = statementnumber(frame, line)

Return the index of the first statement in `frame`'s `CodeInfo` that corresponds to `line`.
"""
function statementnumber(framecode::JuliaFrameCode, line)
    lineidx = searchsortedfirst(framecode.code.linetable, line; by=lin->isa(lin,Integer) ? lin : lin.line)
    1 <= lineidx <= length(framecode.code.linetable) || throw(ArgumentError("line $line not found in $(framecode.scope)"))
    return searchsortedfirst(framecode.code.codelocs, lineidx)
end
statementnumber(frame::JuliaStackFrame, line) = statementnumber(frame.code, line)

"""
    framecode, stmtidx = statementnumber(method, line)

Return the index of the first statement in `framecode` that corresponds to the given `line` in `method`.
"""
function statementnumber(method::Method, line; line1=whereis(method)[2])
    linec = line - line1 + method.line  # line number at time of compilation
    framecode = get_framecode(method)
    return framecode, statementnumber(framecode, linec)
end
