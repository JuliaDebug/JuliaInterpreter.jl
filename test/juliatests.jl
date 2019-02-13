using JuliaInterpreter
using Test, Random, InteractiveUtils

if !isdefined(Main, :read_and_parse)
    include("utils.jl")
end

const juliadir = dirname(dirname(Sys.BINDIR))
const testdir = joinpath(juliadir, "test")
if isdir(testdir)
    include(joinpath(testdir, "choosetests.jl"))
else
    @error "Julia's test/ directory not found, can't run Julia tests"
end

nstmts = 10^4  # very quick, aborts a lot
i = 1
while i <= length(ARGS)
    global i
    a = ARGS[i]
    if a == "--nstmts"
        global nstmts = parse(Int, ARGS[i+1])
        deleteat!(ARGS, i:i+1)
    else
        i += 1
    end
end

module JuliaTests
using Test
end

@testset "Julia tests" begin
    # To do this efficiently, certain methods must be run in Compiled mode
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

    function runtest(frame, nstmts)
        stack = JuliaStackFrame[]
        # empty!(JuliaInterpreter.framedict)
        # empty!(JuliaInterpreter.genframedict)
        ret, nstmts = limited_finish_and_return!(stack, frame, nstmts, true)
        return ret
    end
    function dotest!(test, nstmts)
        println("Working on ", test, "...")
        fullpath = joinpath(testdir, test)*".jl"
        ex = read_and_parse(fullpath)
        # so `include` works properly, we have to set up the relative path
        oldpath = current_task().storage[:SOURCE_PATH]
        if isexpr(ex, :error)
            @error "error parsing $test: $ex"
        else
            local ts, aborts
            try
                current_task().storage[:SOURCE_PATH] = fullpath
                ts = Test.DefaultTestSet(test)
                Test.push_testset(ts)
                docexprs, aborts = lower_incrementally(frame->runtest(frame, nstmts), JuliaTests, ex)
                # Core.eval(JuliaTests, ex)
                println("Finished ", test)
            finally
                current_task().storage[:SOURCE_PATH] = oldpath
            end
            return ts, aborts
        end
    end

    allts = Test.DefaultTestSet[]
    allaborts = Vector{LineNumberNode}[]
    succeeded = Bool[]
    if isdir(testdir)
        tests, _ = choosetests(ARGS)
        for test in tests
            try
                ts, aborts = dotest!(test, nstmts)
                push!(allts, ts)
                push!(allaborts, aborts)
                push!(succeeded, true)
            catch err
                push!(allts, Test.DefaultTestSet(test))
                push!(allaborts, LineNumberNode[])
                push!(succeeded, false)
            end
        end
    end
    open("results.md", "w") do io
        versioninfo(io)
        println(io, "Maximum number of statements per lowered expression: ", nstmts)
        println(io)
        println(io, "| Test file | Passes | Fails | Errors | Broken | Aborted blocks |")
        println(io, "| --------- | ------:| -----:| ------:| ------:| --------------:|")
        for (test, ts, aborts, succ) in zip(tests, allts, allaborts, succeeded)
            if succ
                passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = Test.get_test_counts(ts)
                naborts = length(aborts)
                println(io, "| ", test, " | ", passes+c_passes, " | ", fails+c_fails, " | ", errors+c_errors, " | ", broken+c_broken, " | ", naborts, " |")
            else
                println(io, "| ", test, " | X | X | X | X | X |")
            end
        end
    end
end
