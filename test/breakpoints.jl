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
    stack = JuliaStackFrame[]
    frame = JuliaInterpreter.enter_call(loop_radius2, 10)
    halfthresh = loop_radius2(5)
    JuliaInterpreter.next_line!(stack, frame)
    JuliaInterpreter.next_line!(stack, frame)
    pc = frame.pc[]
    Breakpoints.breakpoint!(frame.code, pc, :(s > $halfthresh))
    bp = JuliaInterpreter.finish_and_return!(stack, frame)
    @test isa(bp, Breakpoints.BreakpointRef)
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

    # break on error
    inner(x) = error("oops")
    outer() = inner(1)
    JuliaInterpreter.break_on_error[] = true
    stack = JuliaStackFrame[]
    frame = JuliaInterpreter.enter_call(outer)
    bp = JuliaInterpreter.finish_and_return!(stack, frame)
    @test bp.err == ErrorException("oops")
    @test length(stack) >= 2
    @test stack[1].code.scope.name == :outer
    @test stack[2].code.scope.name == :inner
end
