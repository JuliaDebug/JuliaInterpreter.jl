using JuliaInterpreter
using JuliaInterpreter: JuliaStackFrame, JuliaProgramCounter, @lookup
using JuliaInterpreter: finish_and_return!, @lookup, evaluate_call!, _step_expr!,
                        do_assignment!, getlhs, isassign, pc_expr, handle_err, get_return,
                        moduleof, prepare_thunk
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

struct Aborted    # for signaling that some statement or test blocks were interrupted
    at::Core.LineInfoNode
end

function Aborted(frame::JuliaStackFrame, pc)
    lineidx = frame.code.code.codelocs[convert(Int, pc)]
    return Aborted(frame.code.code.linetable[lineidx])
end

"""
    ret, nstmtsleft = evaluate_limited!(stack, frame, nstmts, istoplevel::Bool=true)

Run `frame` until one of:
- execution terminates normally (`ret = Some{Any}(val)`, where `val` is the returned value of `frame`)
- if `istoplevel` and a `thunk` or `method` expression is encountered (`ret = nothing`)
- more than `nstmts` have been executed (`ret = Aborted(lin)`, where `lnn` is the `LineInfoNode` of termination).
"""
function evaluate_limited!(stack, frame::JuliaStackFrame, nstmts::Int, pc::JuliaProgramCounter, istoplevel::Bool)
    refnstmts = Ref(nstmts)
    limexec!(s,f) = limited_exec!(s, f, refnstmts, istoplevel)
    # The following is like finish!, except we intercept :call expressions so that we can run them
    # with limexec! rather than the default finish_and_return!
    while nstmts > 0
        stmt = pc_expr(frame, pc)
        if isa(stmt, Expr)
            if stmt.head == :call && !isa(stack, Compiled)
                refnstmts[] = nstmts
                try
                    rhs = evaluate_call!(stack, frame, stmt, pc; exec! = limexec!)
                    isa(rhs, Aborted) && return rhs, refnstmts[]
                    lhs = getlhs(pc)
                    do_assignment!(frame, lhs, rhs)
                    new_pc = pc + 1
                catch err
                    new_pc = handle_err(frame, err)
                end
                nstmts = refnstmts[]
            elseif stmt.head == :(=) && isexpr(stmt.args[2], :call) && !isa(stack, Compiled)
                refnstmts[] = nstmts
                try
                    rhs = evaluate_call!(stack, frame, stmt.args[2], pc; exec! = limexec!)
                    isa(rhs, Aborted) && return rhs, refnstmts[]
                    do_assignment!(frame, stmt.args[1], rhs)
                    new_pc = pc + 1
                catch err
                    new_pc = handle_err(frame, err)
                end
                nstmts = refnstmts[]
            elseif stmt.head == :thunk
                code = stmt.args[1]
                if length(code.code) == 1 && isexpr(code.code[end], :return) && isexpr(code.code[end].args[1], :method)
                    # Julia 1.2+ puts a :thunk before the start of each method
                    new_pc = pc + 1
                else
                    newframe = prepare_thunk(moduleof(frame), stmt)
                    frame.pc[] = pc
                    push!(stack, frame)
                    refnstmts[] = nstmts
                    ret = limited_exec!(stack, newframe, refnstmts, istoplevel)
                    isa(ret, Aborted) && return ret, refnstmts[]
                    pop!(stack)
                    push!(JuliaInterpreter.junk, newframe)  # rather than going through GC, just re-use it
                    frame.pc[] = pc + 1
                    return nothing, refnstmts[]
                end
            elseif stmt.head == :method && length(stmt.args) == 3
                _step_expr!(stack, frame, stmt, pc, istoplevel)
                frame.pc[] = pc + 1
                return nothing, nstmts - 1
            else
                # try
                    new_pc = _step_expr!(stack, frame, stmt, pc, istoplevel)
                # catch err
                #     dump(stmt)
                #     @show istoplevel pc stmt frame
                #     rethrow(err)
                # end
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
    if nstmts == 0 && !isexpr(stmt, :return)
        ret = Aborted(frame, pc)
        return ret, nstmts
    end
    ret = get_return(frame, pc)
    return Some{Any}(ret), nstmts
end
evaluate_limited!(stack, frame::JuliaStackFrame, nstmts::Int, istoplevel::Bool=true) =
    evaluate_limited!(stack, frame, nstmts, frame.pc[], istoplevel)
evaluate_limited!(stack, modex::Tuple{Module,Expr}, nstmts::Int, istoplevel::Bool=true) =
    Some{Any}(Core.eval(modex...)), nstmts

function limited_exec!(stack, newframe, refnstmts, istoplevel)
    ret, nleft = evaluate_limited!(stack, newframe, refnstmts[], newframe.pc[], istoplevel)
    refnstmts[] = nleft
    return isa(ret, Aborted) ? ret : something(ret)
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
