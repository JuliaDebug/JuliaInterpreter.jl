using ASTInterpreter2
using Test
using REPL

using DebuggerFramework

@testset "Main tests" begin
    include("utils.jl")
    include("evaling.jl")
    include("stepping.jl")
    include("interpret.jl")
    include("misc.jl")
    include("toplevel.jl")
end
include("ui.jl")
