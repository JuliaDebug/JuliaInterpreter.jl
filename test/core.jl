using JuliaInterpreter
using Test

@testset "core" begin
    @test JuliaInterpreter.is_quoted_type(QuoteNode(Int32), :Int32)
    @test !JuliaInterpreter.is_quoted_type(QuoteNode(Int32), :Int64)
    @test !JuliaInterpreter.is_quoted_type(QuoteNode(Int32(0)), :Int32)
    @test !JuliaInterpreter.is_quoted_type(Int32, :Int32)
end
