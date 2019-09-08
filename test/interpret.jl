using JuliaInterpreter
using JuliaInterpreter: enter_call_expr
using Test, InteractiveUtils, CodeTracking
using Mmap

module Isolated end

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

# generators that throw before returning the body expression
@test_throws ArgumentError("input tuple of length 3, requested 2") @interpret Base.fill_to_length((1,2,3), -1, Val(2))

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
@test !JuliaInterpreter.is_vararg_type(Union{})
frame = JuliaInterpreter.prepare_thunk(Main, :(Vararg.body.body.name))
@test JuliaInterpreter.finish_and_return!(frame, true) === Vararg.body.body.name
frame = JuliaInterpreter.prepare_thunk(Base, :(Union{AbstractChar,Tuple{Vararg{<:AbstractChar}},AbstractVector{<:AbstractChar},Set{<:AbstractChar}}))
@test JuliaInterpreter.finish_and_return!(frame, true) isa Union

# issue #8
ex = quote
    if sizeof(JLOptions) === ccall(:jl_sizeof_jl_options, Int, ())
    else
        ccall(:jl_throw, Cvoid, (Any,), "Option structure mismatch")
    end
end
frame = JuliaInterpreter.prepare_thunk(Base, ex)
JuliaInterpreter.finish_and_return!(frame, true)

# ccall with two Symbols
ex = quote
    @testset "Some tests" begin
       @test 2 > 1
    end
end
frame = JuliaInterpreter.prepare_thunk(Main, ex)
JuliaInterpreter.finish_and_return!(frame, true)

@test @interpret Base.Math.DoubleFloat64(-0.5707963267948967, 4.9789962508669555e-17).hi ≈ -0.5707963267948967

# ccall with cfunction
fcfun(x::Int, y::Int) = 1
ex = quote   # in lowered code, cf is a Symbol
    cf = @eval @cfunction(fcfun, Int, (Int, Int))
    ccall(cf, Int, (Int, Int), 1, 2)
end
frame = JuliaInterpreter.prepare_thunk(Main, ex)
@test JuliaInterpreter.finish_and_return!(frame, true) == 1
ex = quote
    let   # in lowered code, cf is a SlotNumber
        cf = @eval @cfunction(fcfun, Int, (Int, Int))
        ccall(cf, Int, (Int, Int), 1, 2)
    end
end
frame = JuliaInterpreter.prepare_thunk(Main, ex)
@test JuliaInterpreter.finish_and_return!(frame, true) == 1
function cfcfun()
    cf = @cfunction(fcfun, Int, (Int, Int))
    ccall(cf, Int, (Int, Int), 1, 2)
end
@test @interpret(cfcfun()) == 1

# From Julia's test/ambiguous.jl. This tests whether we renumber :enter statements correctly.
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Int, y::Int) = 4
ambig(x::Number, y) = 5
ex = quote
    let
        cf = @eval @cfunction(ambig, Int, (UInt8, Int))
        @test_throws(MethodError, ccall(cf, Int, (UInt8, Int), 1, 2))
    end
end
frame = JuliaInterpreter.prepare_thunk(Main, ex)
JuliaInterpreter.finish_and_return!(frame, true)

# Core.Compiler
ex = quote
    length(code_typed(fcfun, (Int, Int)))
end
frame = JuliaInterpreter.prepare_thunk(Main, ex)
@test JuliaInterpreter.finish_and_return!(frame, true) == 1

# copyast
ex = quote
    struct CodegenParams
        cached::Cint

        track_allocations::Cint
        code_coverage::Cint
        static_alloc::Cint
        prefer_specsig::Cint

        module_setup::Any
        module_activation::Any
        raise_exception::Any
        emit_function::Any
        emitted_function::Any

        CodegenParams(;cached::Bool=true,
                       track_allocations::Bool=true, code_coverage::Bool=true,
                       static_alloc::Bool=true, prefer_specsig::Bool=false,
                       module_setup=nothing, module_activation=nothing, raise_exception=nothing,
                       emit_function=nothing, emitted_function=nothing) =
            new(Cint(cached),
                Cint(track_allocations), Cint(code_coverage),
                Cint(static_alloc), Cint(prefer_specsig),
                module_setup, module_activation, raise_exception,
                emit_function, emitted_function)
    end
end
frame = JuliaInterpreter.prepare_thunk(Isolated, ex)
JuliaInterpreter.finish_and_return!(frame, true)
@test Isolated.CodegenParams(cached=false).cached === Cint(false)

# cglobal
val = @interpret(BigInt())
@test isa(val, BigInt) && val == 0
@test isa(@interpret(Base.GMP.version()), VersionNumber)

# "correct" line numbers
defline = @__LINE__() + 1
function f(x)
    x = 2x
    # comment
    # comment
    x = 2x
    # comment
    return x*x
end
frame = JuliaInterpreter.enter_call(f, 3)
@test whereis(frame, 1)[2] == defline + 1
@test whereis(frame, 3)[2] == defline + 4
@test whereis(frame, 5)[2] == defline + 6
m = which(iterate, Tuple{Dict}) # this method has `nothing` as its first statement and codeloc == 0
framecode = JuliaInterpreter.get_framecode(m)
@test JuliaInterpreter.linenumber(framecode, 1) == m.line

# issue #28
let a = ['0'], b = ['a']
    @test @interpret(vcat(a, b)) == vcat(a, b)
end

# issue #51
if isdefined(Core.Compiler, :SNCA)
    ci = @code_lowered gcd(10, 20)
    cfg = Core.Compiler.compute_basic_blocks(ci.code)
    @test isa(@interpret(Core.Compiler.SNCA(cfg)), Vector{Int})
end

# llvmcall
function add1234(x::Tuple{Int32,Int32,Int32,Int32})
    Base.llvmcall("""%3 = extractvalue [4 x i32] %0, 0
      %4 = extractvalue [4 x i32] %0, 1
      %5 = extractvalue [4 x i32] %0, 2
      %6 = extractvalue [4 x i32] %0, 3
      %7 = extractvalue [4 x i32] %1, 0
      %8 = extractvalue [4 x i32] %1, 1
      %9 = extractvalue [4 x i32] %1, 2
      %10 = extractvalue [4 x i32] %1, 3
      %11 = add i32 %3, %7
      %12 = add i32 %4, %8
      %13 = add i32 %5, %9
      %14 = add i32 %6, %10
      %15 = insertvalue [4 x i32] undef, i32 %11, 0
      %16 = insertvalue [4 x i32] %15, i32 %12, 1
      %17 = insertvalue [4 x i32] %16, i32 %13, 2
      %18 = insertvalue [4 x i32] %17, i32 %14, 3
      ret [4 x i32] %18""",Tuple{Int32,Int32,Int32,Int32},
      Tuple{Tuple{Int32,Int32,Int32,Int32},Tuple{Int32,Int32,Int32,Int32}},
        (Int32(1),Int32(2),Int32(3),Int32(4)),
        x)
end
@test @interpret(add1234(map(Int32,(2,3,4,5)))) === map(Int32,(3,5,7,9))

# issue #74
let A = [1]
    wkd = WeakKeyDict()
    @interpret setindex!(wkd, 2, A)
    @test wkd[A] == 2
end

# issue #76
let TT = Union{UInt8, Int8}
    a = TT[0x0, 0x1]
    pa = pointer(a)
    @interpret unsafe_store!(pa, 0x1, 2)
end

# issue #92
let x = Core.TypedSlot(1, Any)
    f(x) = objectid(x)
    @test isa(@interpret(f(x)), UInt)
end

# issue #98
x98 = 5
function f98()
    global x98
    x98 = 7
    return nothing
end
@interpret f98()
@test x98 == 7

# issue #106
function f106()
    n = tempname()
    w = open(n, "a")
    write(w, "A")
    flush(w)
    return true
end
@test @interpret(f106()) == 1
f106b() = rand()
f106c() = disable_sigint(f106b)
function f106d()
    disable_sigint() do
        reenable_sigint(f106b)
    end
end
@interpret f106c()
@interpret f106d()

# issue #113
f113(;x) = x
@test @interpret(f113(;x=[1,2,3])) == f113(;x=[1,2,3])

# Some expression can appear nontrivial but lower to nothing
@test isa(JuliaInterpreter.prepare_thunk(Main, :(@static if ccall(:jl_get_UNAME, Any, ()) == :NoOS 1+1 end)), Nothing)
@test isa(JuliaInterpreter.prepare_thunk(Main, :(Base.BaseDocs.@kw_str "using")), Nothing)

@testset "locals" begin
    f_locals(x::Int64, y::T, z::Vararg{Symbol}) where {T} = x
    frame = JuliaInterpreter.enter_call(f_locals, Int64(1), 2.0, :a, :b)
    locals = JuliaInterpreter.locals(frame)
    @test JuliaInterpreter.Variable(Int64(1), :x, false) in locals
    @test JuliaInterpreter.Variable(2.0, :y, false) in locals
    @test JuliaInterpreter.Variable((:a, :b), :z, false) in locals
    @test JuliaInterpreter.Variable(Float64, :T, true) in locals

    function f_multi(x)
        c = x
        x = 2
        x = 3
        x = 4
        return x
    end
    frame = JuliaInterpreter.enter_call(f_multi, 1)
    nlocals = length(frame.framedata.locals)
    @test_throws UndefVarError JuliaInterpreter.lookup_var(frame, JuliaInterpreter.SlotNumber(nlocals))
    stack = [frame]
    locals = JuliaInterpreter.locals(frame)
    @test length(locals) == 2
    @test JuliaInterpreter.Variable(1, :x, false) in locals
    JuliaInterpreter.step_expr!(stack, frame)
    JuliaInterpreter.step_expr!(stack, frame)
    locals = JuliaInterpreter.locals(frame)
    @test length(locals) == 3
    @test JuliaInterpreter.Variable(1, :c, false) in locals
    JuliaInterpreter.step_expr!(stack, frame)
    locals = JuliaInterpreter.locals(frame)
    @test length(locals) == 3
    @test JuliaInterpreter.Variable(2, :x, false) in locals
    JuliaInterpreter.step_expr!(stack, frame)
    locals = JuliaInterpreter.locals(frame)
    @test length(locals) == 3
    @test JuliaInterpreter.Variable(3, :x, false) in locals
end

@testset "getfield replacements" begin
    f_gf(x) = false ? some_undef_var_zzzzzzz : x
    @test @interpret f_gf(2) == 2

    function g_gf()
        eval(:(z = 2))
        return z
    end
    @test @interpret g_gf() == 2

    global q_gf = 0
    function h_gf()
        eval(:(q_gf = 2))
        return q_gf
    end
    @test @interpret h_gf() == 2

    # https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/267
    function test_never_different(x)
        if x < 5
            for g in never_defined
                print(g)
            end
        end
    end
    @test @interpret(test_never_different(10)) === nothing

end

# https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/130
@testset "vararg handling" begin
    method_c1(x::Float64, s::AbstractString...) = true
    buf = IOBuffer()
    me = Base.MethodError(method_c1,(1, 1, ""))
    @test (@interpret Base.show_method_candidates(buf, me)) == nothing

    varargidentity(x) = x
    x = Union{Array{UInt8,N},Array{Int8,N}} where N
    @test isa(JuliaInterpreter.prepare_call(varargidentity, [varargidentity, x])[1], JuliaInterpreter.FrameCode)
end

# https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/141
@test @interpret get(ENV, "THIS_IS_NOT_DEFINED_1234", "24") == "24"

# Test return value of whereis
f() = nothing
fr = JuliaInterpreter.enter_call(f)
file, line = JuliaInterpreter.whereis(fr)
@test file == @__FILE__
@test line == (@__LINE__() - 4)

# Test path to files in stdlib
fr = JuliaInterpreter.enter_call(Test.eval, 1)
file, line = JuliaInterpreter.whereis(fr)
@test isfile(file)
@test isfile(JuliaInterpreter.getfile(fr.framecode.src.linetable[1]))
@test occursin(Sys.STDLIB, repr(fr))

# Test undef sparam (https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/165)
function foo(x::T) where {T <: AbstractString, S <: AbstractString}
    return S
end
e = try
        @interpret foo("")
    catch err
        err
    end
@test e isa UndefVarError
@test e.var == :S
# https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/200
locs = JuliaInterpreter.locals(JuliaInterpreter.enter_call(foo, ""))
@test length(locs) == 3 # #self# + 2 variables
@test JuliaInterpreter.Variable("", :x, false) in locs
@test JuliaInterpreter.Variable(String, :T, true) in locs

# Test interpreting subtypes finishes in a reasonable time
@test @interpret subtypes(Integer) == subtypes(Integer)
@test @interpret subtypes(Main, Integer) == subtypes(Main, Integer)
@test (@elapsed @interpret subtypes(Integer)) < 30
@test (@elapsed @interpret subtypes(Main, Integer)) < 30

# Test showing stacktraces from frames
g_1(x) = g_2(x)
g_2(x) = g_3(x)
g_3(x) = error("foo")
line_g = @__LINE__
try
    break_on(:error)
    frame, bp = @interpret g_1(2.0)
    stacktrace_lines = split(sprint(Base.display_error, bp.err, leaf(frame)), '\n')
    @test occursin(string("ERROR: ", sprint(showerror, ErrorException("foo"))), stacktrace_lines[1])
    @test occursin("[1] error(::String) at error.jl:", stacktrace_lines[3])
    @test occursin("[2] g_3(::Float64) at $(@__FILE__):$(line_g - 1)", stacktrace_lines[4])
    @test occursin("[3] g_2(::Float64) at $(@__FILE__):$(line_g - 2)", stacktrace_lines[5])
    @test occursin("[4] g_1(::Float64) at $(@__FILE__):$(line_g - 3)", stacktrace_lines[6])
finally
    break_off(:error)
end

# https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/154
q = QuoteNode([1])
@test @interpret deepcopy(q) == q

# Check #args for builtins (#217)
f217() = <:(Float64, Float32, Float16)
@test_throws ArgumentError @interpret(f217())

# issue #220
function hash220(x::Tuple{Ptr{UInt8},Int}, h::UInt)
    h += Base.memhash_seed
    ccall(Base.memhash, UInt, (Ptr{UInt8}, Csize_t, UInt32), x[1], x[2], h % UInt32) + h
end
@test @interpret(hash220((Ptr{UInt8}(0),0), UInt(1))) == hash220((Ptr{UInt8}(0),0), UInt(1))

# ccall with type parameters
@test (@interpret Base.unsafe_convert(Ptr{Int}, [1,2])) isa Ptr{Int}

# ccall with call to get the pointer
cf = [@cfunction(fcfun, Int, (Int, Int))]
function call_cf()
    ccall(cf[1], Int, (Int, Int), 1, 2)
end
@test (@interpret call_cf()) == call_cf()
frame = JuliaInterpreter.enter_call(call_cf)
@test frame.framecode.methodtables[2] == Compiled()

# ccall with integer static parameter
f_N() =  Array{Float64, 4}(undef, 1, 3, 2, 1)
@test (@interpret f_N()) isa Array{Float64, 4}

f() = ccall((:clock, "libc"), Int32, ())
# See that the method gets compiled
try @interpret f()
catch
end
frame = JuliaInterpreter.enter_call(f)
@test frame.framecode.methodtables[1] == Compiled()

# https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/194
f() =  Meta.lower(Main, Meta.parse("(a=1,0)"))
@test @interpret f() == f()

# Test for vararg ccalls (used by mmap)
function f_mmap()
    tmp = tempname()
    local b_mmap
    try
        x = rand(10)
        write(tmp, x)
        b_mmap = Mmap.mmap(tmp, Vector{Float64})
        @test b_mmap == x
    finally
        finalize(b_mmap)
        rm(tmp)
    end
end
@interpret f_mmap()

# parametric llvmcall (issues #112 and #288)
module VecTest
    using Tensors
    Vec{N,T} = NTuple{N,VecElement{T}}
    # The following test mimic SIMD.jl
    const _llvmtypes = Dict{DataType, String}(
        Float64 => "double",
        Float32 => "float",
        Int32 => "i32",
        Int64 => "i64"
    )
    @generated function vecadd(x::Vec{N, T}, y::Vec{N, T}) where {N, T}
        llvmT = _llvmtypes[T]
        func = T <: AbstractFloat ? "fadd" : "add"
        exp = """
        %3 = $(func) <$(N) x $(llvmT)> %0, %1
        ret <$(N) x $(llvmT)> %3
        """
        return quote
            Base.@_inline_meta
            Core.getfield(Base, :llvmcall)($exp, Vec{$N, $T}, Tuple{Vec{$N, $T}, Vec{$N, $T}}, x, y)
        end
    end
    f() = 1.0 * one(Tensor{2,3})
end
let a = (VecElement{Float64}(1.0), VecElement{Float64}(2.0))
    @test @interpret(VecTest.vecadd(a, a)) == VecTest.vecadd(a, a)
end
@test @interpret(VecTest.f()) == [1 0 0; 0 1 0; 0 0 1]

# Test exception type for undefined variables
f() = s = s + 1
@test_throws UndefVarError @interpret f()

# Handling of SSAValues
function f()
    z = [Core.SSAValue(5),]
    repr(z[1])
end
@test @interpret f() == f()

# Test JuliaInterpreter version of #265
f(x) = x
g(x) = f(x)
@test (@interpret g(5)) == g(5)
f(x) = x*x
@test (@interpret g(5)) == g(5)

# Regression test https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/300
module CSVTest
    using Test
    using JuliaInterpreter
    @static if sizeof(Int) == 8 # TableReader seems to not work on 32 bit
        using TableReader
        const myfile = "smallcsv.csv"
        @test (@interpret readcsv(myfile)) == readcsv(myfile)
    end
end

# Regression test https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/328
module DataFramesTest
    using Test
    using JuliaInterpreter
    using DataFrames
    function df_debug1()
        df = DataFrame(A=1:3, B=4:6)
        df1 = hcat(df[!,[:A]], df[!,[:B]])
    end
    @test @interpret(df_debug1()) == df_debug1()
end

# issue #330
@test @interpret(Base.PipeEndpoint()) isa Base.PipeEndpoint
