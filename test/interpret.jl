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

# issue #3
@test @interpret(joinpath("/home/julia/base", "sysimg.jl")) == "/home/julia/base/sysimg.jl"
@test @interpret(10.0^4) == 10.0^4
# issue #6
@test @interpret(Array.body.body.name) === Array.body.body.name
@test @interpret(Vararg.body.body.name) === Vararg.body.body.name
frame = JuliaInterpreter.prepare_toplevel(Main, :(Vararg.body.body.name))
@test JuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame, true) === Vararg.body.body.name

ex = quote
    if sizeof(JLOptions) === ccall(:jl_sizeof_jl_options, Int, ())
    else
        ccall(:jl_throw, Cvoid, (Any,), "Option structure mismatch")
    end
end
frame = JuliaInterpreter.prepare_toplevel(Base, ex)
JuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame, true)
