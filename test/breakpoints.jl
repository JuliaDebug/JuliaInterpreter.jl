radius2(x, y) = x^2 + y^2
function loop_radius2(n)
    s = 0
    for i = 1:n
        s += radius2(1, i)
    end
    s
end

@testset "Breakpoints" begin
    breakpoint(radius2)
    stack = JuliaStackFrame[]
    frame = JuliaInterpreter.enter_call(loop_radius2, 2)
    bp = JuliaInterpreter.finish_and_return!(stack, frame)
    @test isa(bp, Breakpoints.BreakpointRef)
    @test length(stack) == 2
    @test stack[end].code.scope == @which radius2(0, 0)
    bp = JuliaInterpreter.finish_stack!(stack)
    @test isa(bp, Breakpoints.BreakpointRef)
    @test length(stack) == 2
    @test JuliaInterpreter.finish_stack!(stack) == loop_radius2(2)

    # Conditional breakpoints
    function runsimple()
        stack = JuliaStackFrame[]
        frame = JuliaInterpreter.enter_call(loop_radius2, 2)
        bp = JuliaInterpreter.finish_and_return!(stack, frame)
        @test isa(bp, Breakpoints.BreakpointRef)
        @test length(stack) == 2
        @test stack[end].code.scope == @which radius2(0, 0)
        @test JuliaInterpreter.finish_stack!(stack) == loop_radius2(2)
    end
    remove()
    breakpoint(radius2, :(y > x))
    runsimple()
    remove()
    @breakpoint radius2(0,0) y>x
    runsimple()
    # Demonstrate the problem that we have with scope
    local_identity(x) = identity(x)
    remove()
    @breakpoint radius2(0,0) y>local_identity(x)
    @test_broken @interpret loop_radius2(2)

    # Conditional breakpoints on local variables
    remove()
    halfthresh = loop_radius2(5)
    @breakpoint loop_radius2(10) 5 s>$halfthresh
    stack, bp = @interpret loop_radius2(10)
    @test isa(bp, Breakpoints.BreakpointRef)
    frame = stack[end]
    s_extractor = eval(Breakpoints.prepare_slotfunction(frame.code, :s))
    @test s_extractor(frame) == loop_radius2(6)
    JuliaInterpreter.finish_stack!(stack)
    @test s_extractor(frame) == loop_radius2(7)
    disable(bp)
    @test JuliaInterpreter.finish_stack!(stack) == loop_radius2(10)

    # Next line with breakpoints
    function outer(x)
        inner(x)
    end
    function inner(x)
        return 2
    end
    breakpoint(inner)
    stack = JuliaStackFrame[]
    frame = JuliaInterpreter.enter_call(outer, 2)
    bp = JuliaInterpreter.next_line!(stack, frame)
    @test isa(bp, Breakpoints.BreakpointRef)
    @test JuliaInterpreter.finish_stack!(stack) == 2

    # Direct return
    @breakpoint gcd(1,1) a==5
    @test @interpret(gcd(10,20)) == 10
    # FIXME: even though they pass, these tests break Test!
    # stack, bp = @interpret gcd(5, 20)
    # @test length(stack) == 1 && isa(stack[1], JuliaStackFrame)
    # @test isa(bp, Breakpoints.BreakpointRef)
    remove()

    # break on error
    try
        JuliaInterpreter.break_on_error[] = true

        inner(x) = error("oops")
        outer() = inner(1)
        stack = JuliaStackFrame[]
        frame = JuliaInterpreter.enter_call(outer)
        bp = JuliaInterpreter.finish_and_return!(stack, frame)
        @test bp.err == ErrorException("oops")
        @test length(stack) >= 2
        @test stack[1].code.scope.name == :outer
        @test stack[2].code.scope.name == :inner

        f_catch() = try error(); catch; return 2; end
        @test @interpret f_catch() == 2

    finally
        JuliaInterpreter.break_on_error[] = false
    end
end
