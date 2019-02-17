using JuliaInterpreter
using Test, Random, InteractiveUtils, Distributed, Dates

# Much of this file is taken from Julia's test/runtests.jl file.

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

function test_path(test)
    t = split(test, '/')
    if t[1] in STDLIBS
        if length(t) == 2
            return joinpath(STDLIB_DIR, t[1], "test", t[2])
        else
            return joinpath(STDLIB_DIR, t[1], "test", "runtests")
        end
    else
        return joinpath(testdir, test)
    end
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

tests, _, exit_on_error, seed = choosetests(ARGS)

function spin_up_workers(n)
    procs = addprocs(n)
    @sync begin
        @async for p in procs
            remotecall_wait(include, p, "utils.jl")
            remotecall_wait(configure_test, p)
        end
    end
    return procs
end

# Really, we're just going to skip all the tests that run on node1
const node1_tests = String[]
function move_to_node1(t)
    if t in tests
        splice!(tests, findfirst(isequal(t), tests))
        push!(node1_tests, t)
    end
    nothing
end
move_to_node1("precompile")
move_to_node1("SharedArrays")
move_to_node1("stress")
move_to_node1("Distributed")

@testset "Julia tests" begin
    nworkers = min(Sys.CPU_THREADS, length(tests))
    println("Using $nworkers workers")
    procs = spin_up_workers(nworkers)
    results = Dict{String,Any}()
    tests0 = copy(tests)
    # @sync begin
        for p in procs
            @async begin
                while length(tests) > 0
                    test = popfirst!(tests)
                    local resp
                    fullpath = test_path(test) * ".jl"
                    try
                        resp = remotecall_fetch(run_test_by_eval, p, test, fullpath, nstmts)
                    catch e
                        isa(e, InterruptException) && return
                        resp = e
                        if isa(e, ProcessExitedException)
                            p = spin_up_workers(1)[1]
                        end
                    end
                    results[test] = resp
                    if resp isa Exception && exit_on_error
                        skipped = length(tests)
                        empty!(tests)
                    end
                end
            end
        end
    # end
    sleep(300)

    open("results.md", "w") do io
        versioninfo(io)
        println(io, "Test run at: ", now())
        println(io)
        println(io, "Maximum number of statements per lowered expression: ", nstmts)
        println(io)
        println(io, "| Test file | Passes | Fails | Errors | Broken | Aborted blocks |")
        println(io, "| --------- | ------:| -----:| ------:| ------:| --------------:|")
        for test in tests0
            haskey(results, test) || (@warn "missing $test"; continue)
            result = results[test]
            if isa(result, Tuple{Test.AbstractTestSet, Vector})
                ts, aborts = result
                passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = Test.get_test_counts(ts)
                naborts = length(aborts)
                println(io, "| ", test, " | ", passes+c_passes, " | ", fails+c_fails, " | ", errors+c_errors, " | ", broken+c_broken, " | ", naborts, " |")
            elseif isa(result, ProcessExitedException)
                println(io, "| ", test, " | ☠️ | ☠️ | ☠️ | ☠️ | ☠️ |")
            else
                println(test, " => ", result)
                println(io, "| ", test, " | X | X | X | X | X |")
            end
        end
    end
end
