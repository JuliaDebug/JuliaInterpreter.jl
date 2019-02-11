using JuliaInterpreter
using Test

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
    stack = JuliaStackFrame[]
    function runtest(frame)
        empty!(stack)
        # empty!(JuliaInterpreter.framedict)
        # empty!(JuliaInterpreter.genframedict)
        return JuliaInterpreter.finish_and_return!(stack, frame, true)
    end
    function dotest!(failed, test)
        println("Working on ", test, "...")
        ex = read_and_parse(joinpath(testdir, test)*".jl")
        if isexpr(ex, :error)
            @warn "error parsing $test: $ex"
        else
            try
                lower_incrementally(runtest, JuliaTests, ex)
                println("Succeeded on ", test)
            catch err
                @show test err
                push!(failed, (test, err))
                # rethrow(err)
            end
        end
    end
    if isdir(testdir)
        tests, _ = choosetests()
        delayed = []
        failed = []
        for test in tests
            if startswith(test, "compiler") || test == "subarray"
                push!(delayed, test)
            else
                dotest!(failed, test)
            end
        end
        for test in delayed
            dotest!(failed, test)
        end
        @show failed
        @test isempty(failed)
    end
end
