using JuliaInterpreter
using Test, Random

if !isdefined(Main, :read_and_parse)
    include("utils.jl")
end

const juliadir = dirname(dirname(Sys.BINDIR))
const testdir = joinpath(juliadir, "test")
if isdir(testdir)
    include(joinpath(testdir, "choosetests.jl"))
else
    @warn "Julia's test/ directory not found, skipping Julia tests"
end

module JuliaTests
using Test
end

@testset "Julia tests" begin
    # To do this efficiently, certain methods must be run in Compiled mode
    cm = JuliaInterpreter.compiled_methods
    empty!(cm)
    push!(cm, which(Test.eval_test, Tuple{Expr, Expr, LineNumberNode}))
    push!(cm, which(Test.finish, Tuple{Test.DefaultTestSet}))
    push!(cm, which(Test.get_testset, Tuple{}))
    push!(cm, which(Test.push_testset, Tuple{Test.AbstractTestSet}))
    push!(cm, which(Test.pop_testset, Tuple{}))
    push!(cm, which(Random.seed!, Tuple{Union{Integer,Vector{UInt32}}}))
    push!(cm, which(copy!, Tuple{Random.MersenneTwister, Random.MersenneTwister}))
    push!(cm, which(copy, Tuple{Random.MersenneTwister}))
    push!(cm, which(Base.include, Tuple{Module, String}))
    push!(cm, which(Base.show_backtrace, Tuple{IO, Vector}))
    push!(cm, which(Base.show_backtrace, Tuple{IO, Vector{Any}}))

    stack = JuliaStackFrame[]
    function runtest(frame)
        empty!(stack)
        # empty!(JuliaInterpreter.framedict)
        # empty!(JuliaInterpreter.genframedict)
        return JuliaInterpreter.finish_and_return!(stack, frame, true)
    end
    function dotest!(test)
        println("Working on ", test, "...")
        fullpath = joinpath(testdir, test)*".jl"
        ex = read_and_parse(fullpath)
        # so `include` works properly, we have to set up the relative path
        oldpath = current_task().storage[:SOURCE_PATH]
        if isexpr(ex, :error)
            @error "error parsing $test: $ex"
        else
            try
                current_task().storage[:SOURCE_PATH] = fullpath
                lower_incrementally(runtest, JuliaTests, ex)
                # Core.eval(JuliaTests, ex)
                println("Finished ", test)
            finally
                current_task().storage[:SOURCE_PATH] = oldpath
            end
        end
    end
    if isdir(testdir)
        tests, _ = choosetests()
        delayed = []
        for test in tests
            if startswith(test, "compiler") || test == "subarray"
                push!(delayed, test)
            else
                dotest!(test)
            end
        end
        for test in delayed
            dotest!(failed, test)
        end
    end
end
