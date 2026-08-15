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
    # The interpolated `quote` block must display as a single statement that carries the
    # quoted code: flisp lowers it to `Expr(:copyast, ...)`, JuliaLowering to a call of
    # `interpolate_expr`; either way the statement's text includes the quoted `println`.
    @test any(str->(occursin(":copyast", str) || occursin("interpolate_expr", str)) && occursin("println", str), lines)

    thunk = Meta.lower(Main, :(return 1+2))
    stmt = thunk.args[1].code[end]::Core.ReturnNode   # the return
    @test stmt.val isa Core.SSAValue

    @test string(JuliaInterpreter.parametric_type_to_expr(Base.Iterators.Stateful{String})) ∈
        ("Base.Iterators.Stateful{String, VS}", "(Base.Iterators).Stateful{String, VS}", "Base.Iterators.Stateful{String, VS, N}")
end
