using InteractiveUtils

const srcpath = joinpath(dirname(@__DIR__), "src")
include(joinpath(srcpath, "generate_builtins.jl"))
open(joinpath(srcpath, "builtins.jl"), "w") do io
    generate_builtins(io)
end
