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

function show_stackloc(io::IO, stack, frame, pc=frame.pc[])
    indent = ""
    for f in stack
        println(io, indent, f.code.scope)
        indent *= "  "
    end
    println(io, indent, frame.code.scope, ", pc = ", convert(Int, pc))
end
function show_stackloc(io::IO, ::Compiled, frame, pc)
    println(io, "No stack, ::Compiled")
    println(io, frame.code.scope, ", pc = ", convert(Int, pc))
end
show_stackloc(stack, frame, pc=frame.pc[]) = show_stackloc(stderr, stack, frame, pc)

function moduleof(x)
    if isa(x, JuliaStackFrame)
        x = x.code.scope
    end
    return isa(x, Module) ? x : x.module
end
Base.nameof(frame::JuliaStackFrame) = isa(frame.code.scope, Method) ? frame.code.scope.name : nameof(frame.code.scope)

function to_function(x)
    if isa(x, GlobalRef)
        getfield(x.mod, x.name)
    else
        x
    end
end

_Typeof(x) = isa(x,Type) ? Type{x} : typeof(x)

function whichtt(tt)
    m = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), tt, typemax(UInt))
    m === nothing && return nothing
    return m.func::Method
end

separate_kwargs(args...; kwargs...) = (args, kwargs.data)

is_loc_meta(expr, kind) = isexpr(expr, :meta) && length(expr.args) >= 1 && expr.args[1] === kind

"""
    is_global_ref(g, mod, name)

Tests whether `g` is equal to `GlobalRef(mod, name)`.
"""
is_global_ref(g, mod, name) = isa(g, GlobalRef) && g.mod === mod && g.name == name

function is_function_def(ex)
    (isexpr(ex, :(=)) && isexpr(ex.args[1], :call)) ||
    isexpr(ex,:function)
end

function is_generated(meth)
    isdefined(meth, :generator)
end

"""
    is_doc_expr(ex)

Test whether expression `ex` is a `@doc` expression.
"""
function is_doc_expr(ex)
    docsym = Symbol("@doc")
    if isexpr(ex, :macrocall)
        a = ex.args[1]
        is_global_ref(a, Core, docsym) && return true
        isa(a, Symbol) && a == docsym && return true
        if isexpr(a, :.)
            mod, name = a.args[1], a.args[2]
            return mod === :Core && isa(name, QuoteNode) && name.value == docsym
        end
    end
    return false
end
