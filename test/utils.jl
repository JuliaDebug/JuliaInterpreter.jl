using JuliaInterpreter: JuliaStackFrame, JuliaProgramCounter, @lookup
using JuliaInterpreter: finish_and_return!, @lookup, evaluate_call!, _step_expr!,
                        do_assignment!, getlhs, isassign, pc_expr, handle_err
using Base.Meta: isexpr
using Test

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
    aborts = LineNumberNode[]
    lower_incrementally!(f, docexprs, aborts, mod, ex)
    return docexprs, aborts
end

lower_incrementally!(@nospecialize(f), docexprs, aborts, mod::Module, ex::Expr) =
    lower_incrementally!(f, docexprs, aborts, Expr(:block), mod, ex)

function lower_incrementally!(@nospecialize(f), docexprs, aborts, lex:: Expr, mod::Module, ex::Expr)
    # lex is the expression we'll lower; it will accumulate LineNumberNodes and a
    # single top-level expression. We split blocks, module defs, etc.
    if ex.head == :toplevel || ex.head == :block
        lower_incrementally!(f, docexprs, aborts, lex, mod, ex.args)
    elseif ex.head == :module
        modname = ex.args[2]::Symbol
        newmod = isdefined(mod, modname) ? getfield(mod, modname) : Core.eval(mod, :(module $modname end))
        lower_incrementally!(f, docexprs, aborts, lex, newmod, ex.args[3])
    elseif isdocexpr(ex) && length(ex.args) >= 4
        docexs = get(docexprs, mod, nothing)
        if docexs === nothing
            docexs = docexprs[mod] = Expr[]
        end
        push!(docexs, ex)
        body = ex.args[4]
        if isa(body, Expr)
            lower_incrementally!(f, docexprs, aborts, lex, mod, body)
        end
    else
        # For map(x->x^2, a) we need to split out the anonymous function so that it
        # gets defined prior to using it (essentially a world-age issue for inference)
        if ex.head == :call || ex.head == :(=) && isa(ex.args[2], Expr) && ex.args[2].head == :call
            fas = split_anonymous!(ex)
            for fa in fas
                lex1 = copy(lex)
                push!(lex1.args, fa)
                lower!(f, docexprs, aborts, mod, lex1)
            end
        end
        push!(lex.args, ex)
        lower!(f, docexprs, aborts, mod, lex)
        empty!(lex.args)
    end
    return docexprs, aborts
end

function lower_incrementally!(@nospecialize(f), docexprs, aborts, lex, mod::Module, args::Vector{Any})
    for a in args
        if isa(a, Expr)
            lower_incrementally!(f, docexprs, aborts, lex, mod, a)
        else
            push!(lex.args, a)
        end
    end
end

function lower!(@nospecialize(f), docexprs, aborts, mod::Module, ex::Expr)
    lwr = Meta.lower(mod, ex)
    if isexpr(lwr, :thunk)
        frame = JuliaInterpreter.prepare_thunk(mod, lwr)
        cts = Test.get_testset()
        ret = Base.invokelatest(f, frame)  # if previous thunks define new methods, we need to update world age
        if isa(ret, Aborted)
            push!(aborts, abortline(ex))
        end
        tts = Test.get_testset()
        while tts != cts
            ttsinner = Test.pop_testset()
            tts = Test.get_testset()
            Test.record(tts, ttsinner)
        end
    elseif isa(lwr, Expr) && (lwr.head == :export || lwr.head == :using || lwr.head == :import)
    elseif isa(lwr, Symbol) || isa(lwr, Nothing)
    else
        @show mod ex
        error("lowering did not produce a :thunk Expr")
    end
    return docexprs, aborts
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

struct Aborted end   # for signaling that some statement or test blocks were interrupted

function abortline(ex::Expr)
    if ex.head == :macrocall
        abortline(ex.args[2])
    elseif ex.head == :block
        i = findfirst(x->isa(x, LineNumberNode), ex.args)
        if i === nothing
            length(ex.args) == 1 && return abortline(ex.args[1])
            error("no LineNumberNodes in ", ex, "\nwith ", length(ex.args), " args")
        end
        abortline(ex.args[i])
    elseif ex.head == :let || ex.head == :for
        abortline(ex.args[2])
    else
        error("unhandled expr head ", ex.head, ":\n", ex)
    end
end
abortline(lnn::LineNumberNode) = lnn

"""
    ret = limited_finish_and_return!(stack, frame, nstmts, istoplevel::Bool)

Run `frame` until execution terminates or more than `nstmts` have been executed,
and pass back the computed return value. `stack` controls call evaluation; `stack = Compiled()`
evaluates :call expressions by normal dispatch, whereas a vector of `JuliaStackFrames`
will use recursive interpretation.
"""
function limited_finish_and_return!(stack, frame, nstmts::Int, pc::JuliaProgramCounter, istoplevel::Bool)
    refnstmts = Ref(nstmts)
    limexec!(s,f) = limited_exec!(s, f, refnstmts)
    # The following is like finish!, except we intercept :call expressions so that we can run them
    # with limexec! rather than the default finish_and_return!
    while nstmts > 0
        stmt = pc_expr(frame, pc)
        if isa(stmt, Expr)
            if stmt.head == :call
                refnstmts[] = nstmts
                try
                    rhs = evaluate_call!(stack, frame, stmt, pc; exec! = limexec!)
                    if isassign(frame, pc)
                        lhs = getlhs(pc)
                        do_assignment!(frame, lhs, rhs)
                    end
                catch err
                    return handle_err(frame, err), refnstmts[]
                end
                nstmts = refnstmts[]
                new_pc = pc + 1
            elseif stmt.head == :(=) && isexpr(stmt.args[2], :call)
                refnstmts[] = nstmts
                try
                    rhs = evaluate_call!(stack, frame, stmt.args[2], pc; exec! = limexec!)
                    do_assignment!(frame, stmt.args[1], rhs)
                catch err
                    return handle_err(frame, err), refnstmts[]
                end
                nstmts = refnstmts[]
                new_pc = pc + 1
            else
                new_pc = _step_expr!(stack, frame, stmt, pc, istoplevel)
                nstmts -= 1
            end
        else
            new_pc = _step_expr!(stack, frame, stmt, pc, istoplevel)
            nstmts -= 1
        end
        new_pc == nothing && break
        pc = new_pc
    end
    frame.pc[] = pc
    # Handle the return
    stmt = pc_expr(frame, pc)
    isexpr(stmt, :return) && return @lookup(frame, (stmt::Expr).args[1]), nstmts
    nstmts == 0 && return Aborted(), nstmts
    error("unexpected return statement ", stmt)
end
limited_finish_and_return!(stack, frame, nstmts::Int, istoplevel::Bool) =
    limited_finish_and_return!(stack, frame, nstmts, frame.pc[], istoplevel)


function limited_exec!(stack, newframe, refnstmts)
    ret, nleft = limited_finish_and_return!(stack, newframe, refnstmts[], newframe.pc[], false)
    refnstmts[] = nleft
    return ret
end
