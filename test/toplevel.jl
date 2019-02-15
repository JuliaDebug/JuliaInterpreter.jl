using JuliaInterpreter, Test

function read_and_parse(filename)
    src = read(filename, String)
    ex = Base.parse_input_line(src; filename=filename)
end

module Toplevel end

@testset "toplevel" begin
    stack = JuliaInterpreter.JuliaStackFrame[]
    frames, _ = JuliaInterpreter.prepare_toplevel(Toplevel, read_and_parse("toplevel_script.jl"))
    for frame in frames
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end

    @test isconst(Toplevel, :StructParent)
    @test isconst(Toplevel, :Struct)
    @test isconst(Toplevel, :MyInt8)

    s = Toplevel.Struct([2.0])

    @test Toplevel.f1(0) == 1
    @test Toplevel.f1(0.0) == 2
    @test Toplevel.f1(0.0f0) == 3
    @test Toplevel.f2("hi") == -1
    @test Toplevel.f2(UInt16(1)) == UInt16
    @test Toplevel.f2(3.2) == 0
    @test Toplevel.f2(view([1,2], 1:1)) == 2
    @test Toplevel.f2([1,2]) == 3
    @test Toplevel.f2(reshape(view([1,2], 1:2), 2, 1)) == 4
    if VERSION >= v"1.1.0"
        @test Toplevel.f3(1, 1) == 1
    end
    @test Toplevel.f3(1, :hi) == 2
    @test Toplevel.f3(UInt16(1), :hi) == Symbol
    @test Toplevel.f3(rand(2, 2), :hi, :there) == 2
    @test_throws MethodError Toplevel.f3([1.0], :hi, :there)
    @test Toplevel.f4(1, 1.0) == 1
    @test Toplevel.f4(1, 1) == Toplevel.f4(1) == 2
    @test Toplevel.f4(UInt(1), "hey", 2) == 3
    @test Toplevel.f4(rand(2,2)) == 2
    @test Toplevel.f5(Int8(1); y=22) == 22
    @test Toplevel.f5(Int16(1)) == 2
    @test Toplevel.f5(Int32(1)) == 3
    @test Toplevel.f5(Int64(1)) == 4
    @test Toplevel.f5(rand(2,2); y=7) == 2
    @test Toplevel.f6(1, "hi"; z=8) == 1
    @test Toplevel.f7(1, (1, :hi)) == 1
    @test Toplevel.f8(0) == 1
    @test Toplevel.f9(3) == 9
    @test Toplevel.f9(3.0) == 3.0
    @test s("hello") == [2.0]
    @test Toplevel.Struct{Float32}(Dict(1=>"two")) == 4
    @test Toplevel.first_two_funcs == (Toplevel.f1, Toplevel.f2)
    if VERSION >= v"1.2.0-DEV.239"
        @test isconst(Toplevel, :first_two_funcs)
    else
        @test_broken isconst(Toplevel, :first_two_funcs)
    end
    @test Toplevel.myint isa Toplevel.MyInt8
    @test_throws UndefVarError Toplevel.ffalse(1)
    @test Toplevel.ftrue(1) == 3
    @test Toplevel.fctrue(0) == 1
    @test_throws UndefVarError Toplevel.fcfalse(0)
    @test !Toplevel.Consts.b2
    @test Toplevel.fb1true(0) == 1
    @test_throws UndefVarError Toplevel.fb1false(0)
    @test Toplevel.fb2false(0) == 1
    @test_throws UndefVarError Toplevel.fb2true(0)
    @test Toplevel.fstrue(0) == 1
    @test Toplevel.fouter(1) === 2
    @test Toplevel.feval1(1.0) === 1
    @test Toplevel.feval1(1.0f0) === 1
    @test_throws MethodError Toplevel.feval1(1)
    @test Toplevel.feval2(1.0, Int8(1)) == 2
    @test length(s) === nothing
    @test size(s) === nothing
    @test Toplevel.nbytes(Float32) == 4
    @test Toplevel.typestring(1.0) == "Float64"
    @test Toplevel._feval3(0) == 3
    @test Toplevel.feval_add!(0) == 1
    @test Toplevel.feval_min!(0) == 1
    @test Toplevel.paramtype(Vector{Int8}) == Int8
    @test Toplevel.paramtype(Vector) == Toplevel.NoParam
    @test Toplevel.Inner.g() == 5
    @test Toplevel.Inner.InnerInner.g() == 6

    @test @interpret(Toplevel.f1(0)) == 1
    @test @interpret(Toplevel.f1(0.0)) == 2
    @test @interpret(Toplevel.f1(0.0f0)) == 3
    @test @interpret(Toplevel.f2("hi")) == -1
    @test @interpret(Toplevel.f2(UInt16(1))) == UInt16
    @test @interpret(Toplevel.f2(3.2)) == 0
    @test @interpret(Toplevel.f2(view([1,2], 1:1))) == 2
    @test @interpret(Toplevel.f2([1,2])) == 3
    @test @interpret(Toplevel.f2(reshape(view([1,2], 1:2), 2, 1))) == 4
    if VERSION >= v"1.1.0"
        @test @interpret(Toplevel.f3(1, 1)) == 1
    end
    @test @interpret(Toplevel.f3(1, :hi)) == 2
    @test @interpret(Toplevel.f3(UInt16(1), :hi)) == Symbol
    @test @interpret(Toplevel.f3(rand(2, 2), :hi, :there)) == 2
    @test_throws MethodError @interpret(Toplevel.f3([1.0], :hi, :there))
    @test @interpret(Toplevel.f4(1, 1.0)) == 1
    @test @interpret(Toplevel.f4(1, 1)) == @interpret(Toplevel.f4(1)) == 2
    @test @interpret(Toplevel.f4(UInt(1), "hey", 2)) == 3
    @test @interpret(Toplevel.f4(rand(2,2))) == 2
    @test @interpret(Toplevel.f5(Int8(1); y=22)) == 22
    @test @interpret(Toplevel.f5(Int16(1))) == 2
    @test @interpret(Toplevel.f5(Int32(1))) == 3
    @test @interpret(Toplevel.f5(Int64(1))) == 4
    @test @interpret(Toplevel.f5(rand(2,2); y=7)) == 2
    @test @interpret(Toplevel.f6(1, "hi"; z=8)) == 1
    @test @interpret(Toplevel.f7(1, (1, :hi))) == 1
    @test @interpret(Toplevel.f8(0)) == 1
    @test @interpret(Toplevel.f9(3)) == 9
    @test @interpret(Toplevel.f9(3.0)) == 3.0
    @test @interpret(s("hello")) == [2.0]
    @test @interpret(Toplevel.Struct{Float32}(Dict(1=>"two"))) == 4
    @test_throws UndefVarError @interpret(Toplevel.ffalse(1))
    @test @interpret(Toplevel.ftrue(1)) == 3
    @test @interpret(Toplevel.fctrue(0)) == 1
    @test_throws UndefVarError @interpret(Toplevel.fcfalse(0))
    @test @interpret(Toplevel.fb1true(0)) == 1
    @test_throws UndefVarError @interpret(Toplevel.fb1false(0))
    @test @interpret(Toplevel.fb2false(0)) == 1
    @test_throws UndefVarError @interpret(Toplevel.fb2true(0))
    @test @interpret(Toplevel.fstrue(0)) == 1
    @test @interpret(Toplevel.fouter(1)) === 2
    @test @interpret(Toplevel.feval1(1.0)) === 1
    @test @interpret(Toplevel.feval1(1.0f0)) === 1
    @test_throws MethodError @interpret(Toplevel.feval1(1))
    @test @interpret(Toplevel.feval2(1.0, Int8(1))) == 2
    @test @interpret(length(s)) === nothing
    @test @interpret(size(s)) === nothing
    @test @interpret(Toplevel.nbytes(Float32)) == 4
    @test @interpret(Toplevel.typestring(1.0)) == "Float64"
    @test @interpret(Toplevel._feval3(0)) == 3
    @test @interpret(Toplevel.feval_add!(0)) == 1
    @test @interpret(Toplevel.feval_min!(0)) == 1
    @test @interpret(Toplevel.paramtype(Vector{Int8})) == Int8
    @test @interpret(Toplevel.paramtype(Vector)) == Toplevel.NoParam
    @test @interpret(Toplevel.Inner.g()) == 5
    @test @interpret(Toplevel.Inner.InnerInner.g()) == 6
    @test @interpret(isdefined(Toplevel, :Beat))
    @test @interpret(Toplevel.Beat <: Toplevel.DatesMod.Period)

    # Check that nested expressions are handled appropriately (module-in-block, internal `using`)
    ex = quote
       module Testing
       if true
           using JuliaInterpreter
       end
       end
   end
   frames, _ = JuliaInterpreter.prepare_toplevel(Toplevel, ex)
   for frame in frames
       while true
           JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
       end
   end
   @test Toplevel.Testing.JuliaStackFrame === JuliaStackFrame
end

# incremental interpretation solves world-age problems
# Taken straight from Julia's test/tuple.jl
module IncTest
using Test

struct A_15703{N}
    keys::NTuple{N, Int}
end

struct B_15703
    x::A_15703
end
end

ex = quote
    @testset "issue #15703" begin
        function bug_15703(xs...)
            [x for x in xs]
        end

        function test_15703()
            s = (1,)
            a = A_15703(s)
            ss = B_15703(a).x.keys
            @test ss === s
            bug_15703(ss...)
        end

        test_15703()
    end
end
frames, _ = JuliaInterpreter.prepare_toplevel(IncTest, ex)
stack = JuliaStackFrame[]
for frame in frames
    while true
        JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
    end
end
@test isa(JuliaInterpreter.get_return(frames[end]), Test.DefaultTestSet)

@testset "Enum" begin
    ex = quote
        @enum EnumParent begin
            EnumChild0
            EnumChild1
        end
    end
    frames, _ = JuliaInterpreter.prepare_toplevel(Toplevel, ex)
    stack = JuliaStackFrame[]
    for frame in frames
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end
    @test isa(Toplevel.EnumChild1, Toplevel.EnumParent)
end

module LowerAnon
ret = Ref{Any}(nothing)
end

@testset "Anonymous functions" begin
    ex1 = quote
        f = x -> parse(Int16, x)
        ret[] = map(f, AbstractString[])
    end
    ex2 = quote
        ret[] = map(x->parse(Int16, x), AbstractString[])
    end
    stack = JuliaStackFrame[]
    frames, _ = JuliaInterpreter.prepare_toplevel(LowerAnon, ex1)
    for frame in frames
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end
    @test isa(LowerAnon.ret[], Vector{Int16})
    LowerAnon.ret[] = nothing
    frames, _ = JuliaInterpreter.prepare_toplevel(LowerAnon, ex2)
    for frame in frames
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end
    @test isa(LowerAnon.ret[], Vector{Int16})
    LowerAnon.ret[] = nothing

    ex3 = quote
        const BitIntegerType = Union{map(T->Type{T}, Base.BitInteger_types)...}
    end
    frames, _ = JuliaInterpreter.prepare_toplevel(LowerAnon, ex3)
    for frame in frames
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end
    @test isa(LowerAnon.BitIntegerType, Union)

    ex4 = quote
        y = 3
        z = map(x->x^2+y, [1,2,3])
        y = 4
    end
    frames, _ = JuliaInterpreter.prepare_toplevel(LowerAnon, ex4)
    for frame in frames
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end
    @test LowerAnon.z == [4,7,12]
end
