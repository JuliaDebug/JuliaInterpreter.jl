using ASTInterpreter2
using ASTInterpreter2: enter_call_expr
using Test

function CallTest()
    UnitRange{Int64}(2,2)
end

step_through(enter_call_expr(:($(CallTest)())))

# Properly handle :meta annotations
function MetaTest()
    @Base._pure_meta
    0
end

step_through(enter_call_expr(:($(MetaTest)())))

# Test Vararg handling
function VATest(x...)
    x
end
callVA() = VATest()

step_through(enter_call_expr(:($(VATest)())))

# Test Loops
function LoopTest()
    x = Int[]
    for i = 1:2
        push!(x, i)
    end
    x
end

step_through(enter_call_expr(:($(LoopTest)())))

# Test continue
function ContinueTest()
    x = Int[]
    for i = 1:3
        if true
            push!(x, i)
            continue
        end
        error("Fell through")
    end
    x
end

step_through(enter_call_expr(:($(ContinueTest)())))

#foo() = 1+1
function foo(n)
    x = n+1
    ((BigInt[1 1; 1 0])^x)[2,1]
end


step_through(enter_call_expr(:($(foo)(20))))

# Make sure that Symbols don't get an extra QuoteNode
function foo_sym()
    x = :ok
    typeof(x)
end

@assert step_through(enter_call_expr(:($(foo_sym)()))) == Symbol

# Make sure evalling "new" works with symbols

function new_sym()
  a = :a
  () -> a
end

step_through(enter_call_expr(:($new_sym())))

function summer(A)
    s = zero(eltype(A))
    for a in A
        s += a
    end
    return s
end

A = [0.12, -.99]
frame = ASTInterpreter2.enter_call(summer, A)
frame2 = ASTInterpreter2.enter_call(summer, A)
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
