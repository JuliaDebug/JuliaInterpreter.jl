radius2(x, y) = x^2 + y^2
function loop_radius2(n)
    s = 0
    for i = 1:n
        s += radius2(1, i)
    end
    s
end

tmppath = ""
global tmppath
tmppath, io = mktemp()
print(io, """
function jikwfunc(x, y=0; z="hello")
    a = x + y
    b = z^a
    return length(b)
end
""")
close(io)
include(tmppath)

using JuliaInterpreter, CodeTracking, Test

function stacklength(frame)
    n = 1
    frame = frame.callee
    while frame !== nothing
        n += 1
        frame = frame.callee
    end
    return n
end

struct Squarer end

@testset "Breakpoints" begin
    Δ = CodeTracking.line_is_decl

    breakpoint(radius2)
    frame = JuliaInterpreter.enter_call(loop_radius2, 2)
    bp = JuliaInterpreter.finish_and_return!(frame)
    @test isa(bp, JuliaInterpreter.BreakpointRef)
    @test stacklength(frame) == 2
    @test leaf(frame).framecode.scope == @which radius2(0, 0)
    bp = JuliaInterpreter.finish_stack!(frame)
    @test isa(bp, JuliaInterpreter.BreakpointRef)
    @test stacklength(frame) == 2
    @test JuliaInterpreter.finish_stack!(frame) == loop_radius2(2)

    # Conditional breakpoints
    function runsimple()
        frame = JuliaInterpreter.enter_call(loop_radius2, 2)
        bp = JuliaInterpreter.finish_and_return!(frame)
        @test isa(bp, JuliaInterpreter.BreakpointRef)
        @test stacklength(frame) == 2
        @test leaf(frame).framecode.scope == @which radius2(0, 0)
        @test JuliaInterpreter.finish_stack!(frame) == loop_radius2(2)
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
    bp = @breakpoint loop_radius2(10) 5 s>$halfthresh
    frame, bpref = @interpret loop_radius2(10)
    @test isa(bpref, JuliaInterpreter.BreakpointRef)
    lframe = leaf(frame)
    s_extractor = eval(JuliaInterpreter.prepare_slotfunction(lframe.framecode, :s))
    @test s_extractor(lframe) == loop_radius2(6)
    JuliaInterpreter.finish_stack!(frame)
    @test s_extractor(lframe) == loop_radius2(7)
    disable(bp)
    @test JuliaInterpreter.finish_stack!(frame) == loop_radius2(10)

    # Return value with breakpoints
    @breakpoint sum([1,2]) any(x->x>4, a)
    val = @interpret sum([1,2,3])
    @test val == 6
    frame, bp = @interpret sum([1,2,5])
    @test isa(frame, Frame) && isa(bp, JuliaInterpreter.BreakpointRef)

    # Next line with breakpoints
    function outer(x)
        inner(x)
    end
    function inner(x)
        return 2
    end
    breakpoint(inner)
    frame = JuliaInterpreter.enter_call(outer, 0)
    bp = JuliaInterpreter.next_line!(frame)
    @test isa(bp, JuliaInterpreter.BreakpointRef)
    @test JuliaInterpreter.finish_stack!(frame) == 2

    # Breakpoints by file/line
    remove()
    method = which(JuliaInterpreter.locals, Tuple{Frame})
    breakpoint(String(method.file), method.line+1)
    frame = JuliaInterpreter.enter_call(loop_radius2, 2)
    ret = @interpret JuliaInterpreter.locals(frame)
    @test isa(ret, Tuple{Frame,JuliaInterpreter.BreakpointRef})
    # Test kwarg method
    remove()
    bp = breakpoint(tmppath, 3)
    frame, bp2 = @interpret jikwfunc(2)
    var = JuliaInterpreter.locals(leaf(frame))
    @test !any(v->v.name === :b, var)
    @test filter(v->v.name === :a, var)[1].value == 2

    # Method with local scope (two slots with same name)
    ln = @__LINE__
    function ftwoslots()
        y = 1
        z = let y = y
                y = y + 2
                rand()
            end
        y = y + 1
        return z
    end
    bp = breakpoint(@__FILE__, ln+5, :(y > 2))
    frame, bp2 = @interpret ftwoslots()
    var = JuliaInterpreter.locals(leaf(frame))
    @test filter(v->v.name === :y, var)[1].value == 3
    remove(bp)
    bp = breakpoint(@__FILE__, ln+8, :(y > 2))
    @test isa(@interpret(ftwoslots()), Float64)

    # Direct return
    @breakpoint gcd(1,1) a==5
    @test @interpret(gcd(10,20)) == 10
    # FIXME: even though they pass, these tests break Test!
    # frame, bp = @interpret gcd(5, 20)
    # @test stacklength(frame) == 1
    # @test isa(bp, JuliaInterpreter.BreakpointRef)
    remove()

    # break on error
    try
        @test_throws ArgumentError("unsupported state :missing") break_on(:missing)
        break_on(:error)

        inner(x) = error("oops")
        outer() = inner(1)
        frame = JuliaInterpreter.enter_call(outer)
        bp = JuliaInterpreter.finish_and_return!(frame)
        @test bp.err == ErrorException("oops")
        @test stacklength(frame) >= 2
        @test frame.framecode.scope.name === :outer
        cframe = frame.callee
        @test cframe.framecode.scope.name === :inner

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
        frame = JuliaInterpreter.enter_call(f_exc_outer);
        v = JuliaInterpreter.finish_and_return!(frame)
        @test v isa ErrorException
        @test stacklength(frame) == 1

        # Break on caught exception when enabled
        break_on(:throw)
        try
            frame = JuliaInterpreter.enter_call(f_exc_outer);
            v = JuliaInterpreter.finish_and_return!(frame)
            @test v isa BreakpointRef
            @test v.err isa ErrorException
            @test v.framecode.scope == @which error()
        finally
            break_off(:throw)
        end
    finally
        break_off(:error)
    end

    # Breakpoint display
    io = IOBuffer()
    frame = JuliaInterpreter.enter_call(loop_radius2, 2)
    bp = JuliaInterpreter.BreakpointRef(frame.framecode, 1)
    @test repr(bp) == "breakpoint(loop_radius2(n) in $(@__MODULE__) at $(@__FILE__):$(3-Δ), line 3)"
    bp = JuliaInterpreter.BreakpointRef(frame.framecode, 0)  # fictive breakpoint
    @test repr(bp) == "breakpoint(loop_radius2(n) in $(@__MODULE__) at $(@__FILE__):$(3-Δ), %0)"
    bp = JuliaInterpreter.BreakpointRef(frame.framecode, 1, ArgumentError("whoops"))
    @test repr(bp) == "breakpoint(loop_radius2(n) in $(@__MODULE__) at $(@__FILE__):$(3-Δ), line 3, ArgumentError(\"whoops\"))"

    # In source breakpointing
    f_outer_bp(x) = g_inner_bp(x)
    function g_inner_bp(x)
        sin(x)
        @bp
        @bp
        @bp
        x = 3
        return 2
    end
    fr, bp = @interpret f_outer_bp(3)
    @test leaf(fr).framecode.scope.name === :g_inner_bp
    @test bp.stmtidx == 3

    # Breakpoints on types
    remove()
    g() = Int(5.0)
    @breakpoint Int(5.0)
    frame, bp = @interpret g()
    @test bp isa BreakpointRef
    @test leaf(frame).framecode.scope === @which Int(5.0)

    # Breakpoint on call overloads
    (::Squarer)(x) = x^2
    squarer = Squarer()
    @breakpoint squarer(2)
    frame, bp = @interpret squarer(3.0)
    @test bp isa BreakpointRef
    @test leaf(frame).framecode.scope === @which squarer(3.0)
end

mktemp() do path, io
    print(io, """
    function somefunc(x, y=0)
        a = x + y
        b = z^a
        return a + b
    end
    """)
    close(io)
    breakpoint(path, 3)
    include(path)
    frame, bp = @interpret somefunc(2, 3)
    @test bp isa BreakpointRef
    @test JuliaInterpreter.whereis(frame) == (path, 3)
    breakpoint(path, 2)
    frame, bp = @interpret somefunc(2, 3)
    @test bp isa BreakpointRef
    @test JuliaInterpreter.whereis(frame) == (path, 2)
    remove()
    # Test relative paths
    mktempdir(dirname(path)) do tmp
        cd(tmp) do
            breakpoint(joinpath("..", basename(path)), 3)
            frame, bp = @interpret somefunc(2, 3)
            @test bp isa BreakpointRef
            @test JuliaInterpreter.whereis(frame) == (path, 3)
            remove()
            breakpoint(joinpath("..", basename(path)), 3)
            cd(homedir()) do
                frame, bp = @interpret somefunc(2, 3)
                @test bp isa BreakpointRef
                @test JuliaInterpreter.whereis(frame) == (path, 3)
            end
        end
    end
end

if tmppath != ""
    rm(tmppath)
end

@testset "toggling" begin
    remove()
    f_break(x::Int) = x
    bp = breakpoint(f_break)
    frame, bpref = @interpret f_break(5)
    @test bpref isa BreakpointRef
    toggle(bp)
    @test (@interpret f_break(5)) == 5
    f_break(x::Float64) = 2x
    @test (@interpret f_break(2.0)) == 4.0
    toggle(bp)
    frame, bpref = @interpret f_break(5)
    @test bpref isa BreakpointRef
    frame, bpref = @interpret f_break(2.0)
    @test bpref isa BreakpointRef
end

using Dates
@testset "breakpoint in stdlibs by path" begin
    m = @which now() - Month(2)
    f = String(m.file)
    l = m.line + 1
    for f in (f, basename(f))
        remove()
        breakpoint(f, l)
        frame, bp = @interpret now() - Month(2)
        @test bp isa BreakpointRef
        @test JuliaInterpreter.whereis(frame)[2] == l
    end
end

@testset "breakpoint in Base by path" begin
    m = @which sin(2.0)
    f = String(m.file)
    l = m.line + 1
    for f in (f, basename(f))
        remove()
        breakpoint(f, l)
        frame, bp = @interpret sin(2.0)
        @test bp isa BreakpointRef
        @test JuliaInterpreter.whereis(frame)[2] == l
    end
end

@testset "breakpoint by type" begin
    remove()
    breakpoint(sin, Tuple{Float64})
    frame, bp = @interpret sin(2.0)
    @test bp isa BreakpointRef
end

const breakpoint_update_hooks = JuliaInterpreter.breakpoint_update_hooks
const on_breakpoints_updated = JuliaInterpreter.on_breakpoints_updated
@testset "hooks" begin
    remove()
    f_break(x) = x

    # Check creating hits hook
    empty!(breakpoint_update_hooks)
    hook_hit = false
    on_breakpoints_updated((f,_)->hook_hit = f == breakpoint)
    orig_bp = breakpoint(f_break)
    @test hook_hit

    # Check re-creating hits remove *and* breakpoint (create)
    empty!(breakpoint_update_hooks)
    hit_remove_old = false
    hit_create_new = false
    hit_other = false  # don't want this
    on_breakpoints_updated() do f, hbp
        if f==remove
            hit_remove_old = hbp === orig_bp
        elseif f==breakpoint
            hit_create_new = hbp !== orig_bp
        else
            hit_other = true
        end
    end
    push!(breakpoint_update_hooks, (f,_)->hook_hit = f == breakpoint)
    bp = breakpoint(f_break)
    @test hit_remove_old
    @test hit_create_new
    @test !hit_other

    @testset "update_states! $op hits hook" for op in (disable, enable, toggle)
        empty!(breakpoint_update_hooks)
        hook_hit = false
        on_breakpoints_updated((f, _) -> hook_hit = f == JuliaInterpreter.update_states!)
        op(bp)
        @test hook_hit
    end

    # Test removing hits hooks
    empty!(breakpoint_update_hooks)
    hook_hit = false
    on_breakpoints_updated((f, _) -> hook_hit = f === remove)
    remove(bp)
    @test hook_hit

    @testset "make sure error in hook function doesn't throw" begin
        empty!(breakpoint_update_hooks)
        on_breakpoints_updated((_, _) -> error("bad hook"))
        @test_logs (:warn, r"hook"i) breakpoint(f_break)
    end
end
# Run outside testset so that if it fails, the hooks get removed. So other tests can pass
empty!(breakpoint_update_hooks)

@testset "toplevel breakpoints" begin
    mktemp() do path, io
        print(io, """
        1+1         # bp
        begin
            2+2
            3+3     # bp
        end
        function foo(x)
            x
            x       # bp
            x
        end
        """)
        close(io)

        expr = Base.parse_input_line(String(read(path)), filename = path)
        exprs = collect(ExprSplitter(Main, expr))

        breakpoint(path, 1)
        breakpoint(path, 4)
        breakpoint(path, 8)

        # breakpoint in top-level line
        mod, ex = exprs[1]
        frame = Frame(mod, ex)
        if VERSION < v"1.2"
            @test_broken JuliaInterpreter.shouldbreak(frame, frame.pc)
        else
            @test JuliaInterpreter.shouldbreak(frame, frame.pc)
        end
        ret = JuliaInterpreter.finish_and_return!(frame, true)
        @test ret === 2

        # breakpoint in top-level block
        mod, ex = exprs[3]
        frame = Frame(mod, ex)
        @test JuliaInterpreter.shouldbreak(frame, frame.pc)
        ret = JuliaInterpreter.finish_and_return!(frame, true)
        @test ret === 6

        # don't break for bp in function definition
        mod, ex = exprs[4]
        frame = Frame(mod, ex)
        @test JuliaInterpreter.shouldbreak(frame, frame.pc) == false
        ret = JuliaInterpreter.finish_and_return!(frame, true)
        @test ret isa Function

        remove()
    end
end
