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

    # Breakpoints by file/line
    if isdefined(Main, :Revise)
        remove()
        method = which(JuliaInterpreter.locals, Tuple{JuliaStackFrame})
        breakpoint(String(method.file), method.line+1)
        frame = JuliaInterpreter.enter_call(loop_radius2, 2)
        ret = @interpret JuliaInterpreter.locals(frame)
        @test isa(bp, Breakpoints.BreakpointRef)
    end

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

        # Don't break on caught exceptions
        function f_exc_outer()
            try
                f_exc_inner()
            catch err
                return err
            end
        end
        function f_exc_inner()
            error()
        end
        stack = JuliaStackFrame[];
        frame = JuliaInterpreter.enter_call(f_exc_outer);
        v = JuliaInterpreter.finish_and_return!(stack, frame)
        @test v isa ErrorException
    finally
        JuliaInterpreter.break_on_error[] = false
    end

    # Breakpoint display
    io = IOBuffer()
    frame = JuliaInterpreter.enter_call(loop_radius2, 2)
    bp = JuliaInterpreter.BreakpointRef(frame.code, 1)
    show(io, bp)
    @test String(take!(io)) == "breakpoint(loop_radius2(n) in $(@__MODULE__) at $(@__FILE__):3, 3)"
    bp = JuliaInterpreter.BreakpointRef(frame.code, 0)  # fictive breakpoint
    show(io, bp)
    @test String(take!(io)) == "breakpoint(loop_radius2(n) in $(@__MODULE__) at $(@__FILE__):3, %0)"
    bp = JuliaInterpreter.BreakpointRef(frame.code, 1, ArgumentError("whoops"))
    show(io, bp)
    @test String(take!(io)) == "breakpoint(loop_radius2(n) in $(@__MODULE__) at $(@__FILE__):3, 3, ArgumentError(\"whoops\"))"
end
