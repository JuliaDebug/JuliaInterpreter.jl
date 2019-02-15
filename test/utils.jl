using JuliaInterpreter
using JuliaInterpreter: JuliaStackFrame, JuliaProgramCounter, @lookup
using JuliaInterpreter: finish_and_return!, @lookup, evaluate_call!, _step_expr!,
                        do_assignment!, getlhs, isassign, pc_expr, handle_err
using Base.Meta: isexpr
using Test, Random

# Execute a frame using Julia's regular compiled-code dispatch for any :call expressions
runframe(frame, pc=frame.pc[]) = Some{Any}(finish_and_return!(Compiled(), frame, pc))

# Execute a frame using the interpreter for all :call expressions (except builtins & intrinsics)
function runstack(frame::JuliaStackFrame, pc=frame.pc[])
    stack = JuliaStackFrame[]
    return Some{Any}(finish_and_return!(stack, frame, pc))
end

## For juliatests.jl

function read_and_parse(filename)
    src = read(filename, String)
    ex = Base.parse_input_line(src; filename=filename)
end

## For running interpreter frames under resource limitations

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
    elseif ex.head == :let || ex.head == :for || ex.head == :if
        abortline(ex.args[2])
    elseif ex.head == :call
        error("aborted while running ", ex)
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

### Functions needed on workers for running tests

function configure_test()
    # To run tests efficiently, certain methods must be run in Compiled mode,
    # in particular those that are used by the Test infrastructure
    cm = JuliaInterpreter.compiled_methods
    empty!(cm)
    push!(cm, which(Test.eval_test, Tuple{Expr, Expr, LineNumberNode}))
    push!(cm, which(Test.get_testset, Tuple{}))
    push!(cm, which(Test.push_testset, Tuple{Test.AbstractTestSet}))
    push!(cm, which(Test.pop_testset, Tuple{}))
    for f in (Test.record, Test.finish)
        for m in methods(f)
            push!(cm, m)
        end
    end
    push!(cm, which(Random.seed!, Tuple{Union{Integer,Vector{UInt32}}}))
    push!(cm, which(copy!, Tuple{Random.MersenneTwister, Random.MersenneTwister}))
    push!(cm, which(copy, Tuple{Random.MersenneTwister}))
    push!(cm, which(Base.include, Tuple{Module, String}))
    push!(cm, which(Base.show_backtrace, Tuple{IO, Vector}))
    push!(cm, which(Base.show_backtrace, Tuple{IO, Vector{Any}}))
end

function runtest(frame, nstmts)
    stack = JuliaStackFrame[]
    # empty!(JuliaInterpreter.framedict)
    # empty!(JuliaInterpreter.genframedict)
    ret, nstmts = limited_finish_and_return!(stack, frame, nstmts, true)
    return ret
end

function dotest(test, fullpath, nstmts)
    println("Working on ", test, "...")
    mod = Core.eval(Main, :(
        module JuliaTests
        using Test, Random
        end
        ))
    ex = read_and_parse(fullpath)
    isexpr(ex, :error) && @error "error parsing $test: $ex"
    # so `include` works properly, we have to set up the relative path
    # oldpath = current_task().storage[:SOURCE_PATH]
    local ts, aborts
    try
        # current_task().storage[:SOURCE_PATH] = fullpath
        cd(dirname(fullpath)) do
            ts = Test.DefaultTestSet(test)
            Test.push_testset(ts)
            docexprs, aborts = lower_incrementally(frame->runtest(frame, nstmts), mod, ex)
            # Core.eval(JuliaTests, ex)
        end
    finally
        # current_task().storage[:SOURCE_PATH] = oldpath
    end
    println("Finished ", test)
    return ts, aborts
end

# Run a test in process id 1 (i.e., the main Julia process)
function dotest1(test, fullpath, nstmts)
    println("Working on ", test, "...")
    mod = Core.eval(Main, :(
        module JuliaTests
        using Test, Random
        end
        ))
    ex = read_and_parse(fullpath)
    isexpr(ex, :error) && @error "error parsing $test: $ex"
    # so `include` works properly, we have to set up the relative path
    oldpath = current_task().storage[:SOURCE_PATH]
    local ts, aborts
    try
        current_task().storage[:SOURCE_PATH] = fullpath
        cd(dirname(fullpath)) do
            ts = Test.DefaultTestSet(test)
            Test.push_testset(ts)
            docexprs, aborts = lower_incrementally(frame->runtest(frame, nstmts), mod, ex)
            # Core.eval(JuliaTests, ex)
        end
    finally
        current_task().storage[:SOURCE_PATH] = oldpath
    end
    println("Finished ", test)
    return ts, aborts
end
