using JuliaInterpreter: JuliaStackFrame, finish_and_return!
using Base.Meta: isexpr

# Execute a frame using Julia's regular compiled-code dispatch for any :call expressions
runframe(frame, pc=frame.pc[]) = Some{Any}(finish_and_return!(Compiled(), frame, pc))

# Execute a frame using the interpreter for all :call expressions (except builtins & intrinsics)
function runstack(frame::JuliaStackFrame, pc=frame.pc[])
    stack = JuliaStackFrame[]
    return Some{Any}(finish_and_return!(stack, frame, pc))
end

## For juliatests.jl

isdocexpr(ex) = isexpr(ex, :macrocall) && (a = ex.args[1]; a isa GlobalRef && a.mod == Core && a.name == Symbol("@doc"))

function read_and_parse(filename)
    src = read(filename, String)
    ex = Base.parse_input_line(src; filename=filename)
end

"""
    docexprs = lower_incrementally(f, mod::Module, ex::Expr)

Lower the sub-expressions of `ex` one-by-one, passing the `frame`s to `f(frame)`.
Any docstring-expressions encountered while processing module `M` (which might be `mod`
or one of its sub-modules) get returned in `docexprs[M]`.
"""
function lower_incrementally(@nospecialize(f), mod::Module, ex::Expr)
    docexprs = Dict{Module,Vector{Expr}}()
    lower_incrementally!(f, docexprs, mod, ex)
    return docexprs
end

lower_incrementally!(@nospecialize(f), docexprs, mod::Module, ex::Expr) =
    lower_incrementally!(f, docexprs, Expr(:block), mod, ex)

function lower_incrementally!(@nospecialize(f), docexprs, lex:: Expr, mod::Module, ex::Expr)
    # lex is the expression we'll lower; it will accumulate LineNumberNodes and a
    # single top-level expression. We split blocks, module defs, etc.
    if ex.head == :toplevel || ex.head == :block
        lower_incrementally!(f, docexprs, lex, mod, ex.args)
    elseif ex.head == :module
        modname = ex.args[2]::Symbol
        newmod = isdefined(mod, modname) ? getfield(mod, modname) : Core.eval(mod, :(module $modname end))
        lower_incrementally!(f, docexprs, lex, newmod, ex.args[3])
    elseif isdocexpr(ex) && length(ex.args) >= 4
        docexs = get(docexprs, mod, nothing)
        if docexs === nothing
            docexs = docexprs[mod] = Expr[]
        end
        push!(docexs, ex)
        body = ex.args[4]
        if isa(body, Expr)
            lower_incrementally!(f, docexprs, lex, mod, body)
        end
    else
        # For map(x->x^2, a) we need to split out the anonymous function so that it
        # gets defined prior to using it (essentially a world-age issue for inference)
        if ex.head == :call || ex.head == :(=) && isa(ex.args[2], Expr) && ex.args[2].head == :call
            fas = split_anonymous!(ex)
            for fa in fas
                lex1 = copy(lex)
                push!(lex1.args, fa)
                lower!(f, docexprs, mod, lex1)
            end
        end
        push!(lex.args, ex)
        lower!(f, docexprs, mod, lex)
        empty!(lex.args)
    end
    return docexprs
end

function lower_incrementally!(@nospecialize(f), docexprs, lex, mod::Module, args::Vector{Any})
    for a in args
        if isa(a, Expr)
            lower_incrementally!(f, docexprs, lex, mod, a)
        else
            push!(lex.args, a)
        end
    end
end

function lower!(@nospecialize(f), docexprs, mod::Module, ex::Expr)
    lwr = Meta.lower(mod, ex)
    if isexpr(lwr, :thunk)
        frame = JuliaInterpreter.prepare_thunk(mod, lwr)
        Base.invokelatest(f, frame)  # if previous thunks define new methods, we need to update world age
    elseif isa(lwr, Expr) && (lwr.head == :export || lwr.head == :using || lwr.head == :import)
    elseif isa(lwr, Symbol) || isa(lwr, Nothing)
    else
        @show mod ex
        error("lowering did not produce a :thunk Expr")
    end
    return docexprs
end

split_anonymous!(ex) = split_anonymous!(Expr[], ex)
function split_anonymous!(fs, ex)
    for (i,a) in enumerate(ex.args)
        if isa(a, Expr)
            if a.head == :->
                gs = gensym()
                push!(fs, Expr(:(=), gs, a))
                ex.args[i] = gs
            else
                split_anonymous!(fs, a)
            end
        end
    end
    return fs
end
