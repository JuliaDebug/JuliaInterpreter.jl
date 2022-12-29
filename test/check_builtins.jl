using Test, DeepDiffs

@static if !Base.GIT_VERSION_INFO.tagged_commit && # only run on nightly
    !Sys.iswindows() # TODO: Understand why this fails, probably some line endings
    @testset "Check builtin.jl consistency" begin
        builtins_path = joinpath(@__DIR__, "..", "src", "builtins.jl")
        old_builtins = read(builtins_path, String)
        new_builtins_path = tempname()
        withenv("BUILTINS_PATH" => new_builtins_path) do
            include("../bin/generate_builtins.jl")
        end
        new_builtins = read(new_builtins_path, String)
        consistent = old_builtins == new_builtins
        if !consistent
            println(deepdiff(old_builtins, new_builtins))
        end
        @test consistent
    end
end
