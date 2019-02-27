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
    @test isa(bp, Breakpoints.Breakpoint)
    @test length(stack) == 2
    @test stack[end].code.scope == @which radius2(0, 0)
    bp = JuliaInterpreter.finish_stack!(stack)
    @test isa(bp, Breakpoints.Breakpoint)
    @test length(stack) == 2
    @test JuliaInterpreter.finish_stack!(stack) == loop_radius2(2)

    # Conditional breakpoints
    Breakpoints.remove()
    breakpoint(radius2, :(y > x))
    stack = JuliaStackFrame[]
    frame = JuliaInterpreter.enter_call(loop_radius2, 2)
    bp = JuliaInterpreter.finish_and_return!(stack, frame)
    @test isa(bp, Breakpoints.Breakpoint)
    @test length(stack) == 2
    @test stack[end].code.scope == @which radius2(0, 0)
    @test JuliaInterpreter.finish_stack!(stack) == loop_radius2(2)

    # Conditional breakpoints on local variables
    Breakpoints.remove()
    stack = JuliaStackFrame[]
    frame = JuliaInterpreter.enter_call(loop_radius2, 10)
    halfthresh = loop_radius2(5)
    JuliaInterpreter.next_line!(stack, frame)
    JuliaInterpreter.next_line!(stack, frame)
    pc = frame.pc[]
    Breakpoints.breakpoint!(frame.code, pc, :(s > $halfthresh))
    bp = JuliaInterpreter.finish_and_return!(stack, frame)
    @test isa(bp, Breakpoints.Breakpoint)
    s_extractor = eval(Breakpoints.prepare_slotfunction(frame.code, :s))
    @test s_extractor(frame) == loop_radius2(6)
    JuliaInterpreter.finish_stack!(stack)
    @test s_extractor(frame) == loop_radius2(7)
    Breakpoints.disable(bp)
    @test JuliaInterpreter.finish_stack!(stack) == loop_radius2(10)
end
