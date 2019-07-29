using JuliaInterpreter

if !isdefined(Main, :Serializer)
    include("../src/serializer.jl")
end

function summer(A::AbstractArray{T}) where T
    s = zero(T)
    for a in A
        s += a
    end
    return s
end

a = [1,2,3]
method = @which summer(a)
src = @code_lowered summer(a)
code = Serializer.SerializedCode(method, src)
# Build a frame (currently this must be done manually)
argtypes = Tuple{typeof(summer), typeof(a)}
(ti, lenv) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                    argtypes, method.sig)
frame = Serializer.prepare_frame(code, [summer, a], lenv)

Serializer.print_serialization(stdout, code)

spc = frame.spc
spc = Serializer.step_ser!(nothing, frame, spc, false)
spc = Serializer.step_ser!(nothing, frame, spc, false)
spc = Serializer.step_ser!(nothing, frame, spc, false)
# builtins & intrinsics aren't handled yet, so going any further gives an error
