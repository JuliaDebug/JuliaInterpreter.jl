using JuliaInterpreter
using JuliaInterpreter: enter_call_expr
using Test


function summer(A)
    s = zero(eltype(A))
    for a in A
        s += a
    end
    return s
end

A = [0.12, -.99]
frame = JuliaInterpreter.enter_call(summer, A)
frame2 = JuliaInterpreter.enter_call(summer, A)
@test summer(A) == something(runframe(frame)) == something(runstack(frame2))

A = rand(1000)
@test @interpret(sum(A)) ≈ sum(A)  # note: the compiler can leave things in registers to increase accuracy, doesn't happen with interpreted
fapply() = (Core.apply_type)(Base.NamedTuple, (), Tuple{})
@test @interpret(fapply()) == fapply()
function fbc()
    bc = Broadcast.broadcasted(CartesianIndex, 6, [1, 2, 3])
    copy(bc)
end
@test @interpret(fbc()) == fbc()
@test @interpret(repr("hi")) == repr("hi")  # this tests kwargs and @generated functions

fkw(x::Int8; y=0, z="hello") = y
@test @interpret(fkw(Int8(1); y=22, z="world")) == fkw(Int8(1); y=22, z="world")

# Throwing exceptions across frames
function f_exc_inner()
    error("inner")
end

f_exc_inner2() = f_exc_inner()

const caught = Ref(false)
function f_exc_outer1()
    try
        f_exc_inner()
    catch err    # with an explicit err capture
        caught[] = true
        rethrow(err)
    end
end

function f_exc_outer2()
    try
        f_exc_inner()
    catch        # implicit err capture
        caught[] = true
        rethrow()
    end
end

function f_exc_outer3(f)
    try
        f()
    catch err
        return err
    end
end

@test !caught[]
ret = @interpret f_exc_outer3(f_exc_outer1)
@test ret == ErrorException("inner")
@test caught[]

caught[] = false
ret = @interpret f_exc_outer3(f_exc_outer2)
@test ret == ErrorException("inner")
@test caught[]

caught[] = false
ret = @interpret f_exc_outer3(f_exc_inner2)
@test ret == ErrorException("inner")
@test !caught[]


stc = try f_exc_outer1() catch
    stacktrace(catch_backtrace())
end
sti = try @interpret(f_exc_outer1()) catch
    stacktrace(catch_backtrace())
end
@test_broken stc == sti

# issue #3
@test @interpret(joinpath("/home/julia/base", "sysimg.jl")) == "/home/julia/base/sysimg.jl"
@test @interpret(10.0^4) == 10.0^4
# issue #6
@test @interpret(Array.body.body.name) === Array.body.body.name
@test @interpret(Vararg.body.body.name) === Vararg.body.body.name
frame = JuliaInterpreter.prepare_toplevel(Main, :(Vararg.body.body.name))
@test JuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame, true) === Vararg.body.body.name

# issue #8
ex = quote
    if sizeof(JLOptions) === ccall(:jl_sizeof_jl_options, Int, ())
    else
        ccall(:jl_throw, Cvoid, (Any,), "Option structure mismatch")
    end
end
frame = JuliaInterpreter.prepare_toplevel(Base, ex)
JuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame, true)

# ccall with two Symbols
ex = quote
    @testset "Some tests" begin
       @test 2 > 1
    end
end
frame = JuliaInterpreter.prepare_toplevel(Main, ex)
JuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame, true)
