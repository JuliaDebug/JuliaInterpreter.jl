using Test, DeepDiffs

@static if !Base.GIT_VERSION_INFO.tagged_commit && # only run on nightly
    !Sys.iswindows() # TODO: Understand why this fails, probably some line endings
    @testset "Check builtin.jl consistency" begin
        builtins_path = joinpath(@__DIR__, "..", "src", "builtins.jl")
        old_builtins = read(builtins_path, String)
        new_builtins_dir = mktempdir()
        withenv("JULIAINTERPRETER_BUILTINS_DIR" => new_builtins_dir) do
            include("../bin/generate_builtins.jl")
        end
        new_builtins = read(joinpath(new_builtins_dir, "builtins.jl"), String)
        consistent = old_builtins == new_builtins
        if !consistent
            println(deepdiff(old_builtins, new_builtins))
        end
        @test consistent
    end
end
