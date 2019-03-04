using JuliaInterpreter
using Test

@testset "Main tests" begin
    include("utils.jl")
    include("interpret.jl")
    include("toplevel.jl")
    include("limits.jl")
    include("breakpoints.jl")
end
