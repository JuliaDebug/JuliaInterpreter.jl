using JuliaInterpreter
using Test

@testset "Main tests" begin
    include("utils.jl")
    include("interpret.jl")
    include("toplevel.jl")
end
