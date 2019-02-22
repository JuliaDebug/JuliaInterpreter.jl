using InteractiveUtils

const srcpath = joinpath(dirname(@__DIR__), "src")
include(joinpath(srcpath, "generate_builtins.jl"))
open(joinpath(srcpath, "builtins-julia$(Int(VERSION.major)).$(Int(VERSION.minor)).jl"), "w") do io
    generate_builtins(io)
end
