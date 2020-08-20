using JuliaInterpreter
using Test

@testset "core" begin
    @test JuliaInterpreter.is_quoted_type(QuoteNode(Int32), :Int32)
    @test !JuliaInterpreter.is_quoted_type(QuoteNode(Int32), :Int64)
    @test !JuliaInterpreter.is_quoted_type(QuoteNode(Int32(0)), :Int32)
    @test !JuliaInterpreter.is_quoted_type(Int32, :Int32)

    function buildexpr()
        items = [7, 3]
        ex = quote
            X = $items
            for x in X
                println(x)
            end
        end
        return ex
    end
    frame = JuliaInterpreter.enter_call(buildexpr)
    lines = JuliaInterpreter.framecode_lines(frame.framecode.src)
    # Test that the :copyast ends up on the same line as the println
    if isdefined(Base.IRShow, :show_ir_stmt)   # only works on Julia 1.6 and higher
        @test any(str->occursin(":copyast", str) && occursin("println", str), lines)
    end
end
