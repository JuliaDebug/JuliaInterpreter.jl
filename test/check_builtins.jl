using Test, DeepDiffs

@static if !Base.GIT_VERSION_INFO.tagged_commit # only run on nightly
    @testset "Check builtin.jl consistency" begin
        builtins_path = joinpath(@__DIR__, "..", "src", "builtins.jl")
        old_builtins = read(builtins_path, String)
        include("../bin/generate_builtins.jl")
        new_builtins = read(builtins_path, String)
        consistent = old_builtins == new_builtins
        if !consistent
            println(deepdiff(old_builtins, new_builtins))
        end
        @test consistent
    end
end
