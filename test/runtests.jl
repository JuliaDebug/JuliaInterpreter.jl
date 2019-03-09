using JuliaInterpreter
using Test

@test isempty(detect_ambiguities(JuliaInterpreter, Base, Core))

if !isdefined(@__MODULE__, :read_and_parse)
    include("utils.jl")
end

@testset "Main tests" begin
    include("interpret.jl")
    include("toplevel.jl")
    include("limits.jl")
    include("breakpoints.jl")
end
