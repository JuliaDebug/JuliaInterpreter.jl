module Serializer

using Core: CodeInfo, GotoNode, MethodInstance, SimpleVector
using Base.Meta: isexpr
using ..JuliaInterpreter:
    BreakpointState,
    FrameData,
    Unassigned,
    find_used,
    lookup_global_ref,
    pc_from_spc,
    prepare_framedata

export serialize

# Because we're not sure yet about the tradeoff (or lack thereof)
# between size and performance, we use aliases for a couple of key types
const TokenT  = UInt8   # the eltype of the serialization format (UInt8 or UInt64)
const FIndexT = Int32   # what type we use to index function tables

const max_fixed_args = 4

include("builtins_serializer.jl")

### Serialization format

# This uses a simple format, conceptually implementing a machine with the following
# properties:
# - a tape of instructions called `ser`
# - a single implicit "register" called `ans`
# - the ability to execute operations specific to a particular instruction token
#   in `ser`. Executing these operations may consume future tokens.
#
# Operations are conceptually of 4 categories:
# - loads (which fill `ans` from a variety of sources), encoded by `load*` or literal tokens
# - stores (which put `ans` somewhere more permanent), encoded by `store*` tokens
# - calls (for which the return value is stored in `ans`)
# - control-flow
#
# The implementation of calls is allowed to store data to named local variables or lists,
# thus increasing the temporary storage beyond `ans`.
#
# The serialization of the lowered IR
#    %4 = atan(@3, 2.4)
# might look something like this on the tape:
#    call atan_idx methlist fixedargs 2 loadslot 3 float64 2.4 storessa 4
# where
# - `call` is an instruction token signaling that next operation is a function call
#   (in reality, there are multiple call-type tokens for intrinsics, builtins,
#    generics via the interpreter, generics via ordinary compiled dispatch,
#    `invokelatest`, `Core._apply`, etc.)
# - `atan_idx` is a token representing `atan`. It is encoded as an integer index
#   into a table of functions (the table is maintained by the serializer)
# - `methlist` is a pointer to a local method table, a performance optimization
#   used for avoiding full-blown dispatch (this also stores whether the method should
#   be called via the interpreter or the compiled path)
# - `fixedargs 2` is an indication that this call should use the path optimized for a
#   particular (small) number of arguments, which in this case is 2.
#   An alternative is `listargs args`, which packs an arbitrary number of arguments
#   into a literally-encoded `args::Vector{Any}` stored (via its pointer) in `ser`.
# - `loadslot 3` indicates that the next argument (first argument) is to be loaded
#   from the slots at index 3
# - `float64 2.4` indicates that the next argument (second argument) is a literal
#   value of type `Float64` encoded in `ser` itself.
# - after the second argument, the function call is executed and the result is
#   stored in `ans`
# - `storessa 4` indicates that `ans` should be placed in `%4`.

@enum InterpretToken::TokenT begin
    # Static data (you can think of these as `load*` tokens for literals in `ser`)
    int
    float64
    float32
    nothingtok
    symbolptr        # ptrs to Symbol...
    stringptr        # String
    # Load tokens
    loadssa
    loadslot
    loadparameter    # static parameters
    loadexception    # the most recent exception
    loadglobalref    # GlobalRef
    # Store tokens
    storessa
    storeslot
    storeglobalref
    # Call expressions
    callintrinsic
    callbuiltin
    call             # standard call
    calllatest       # call made by `invokelatest`
    callinvoke       # Core.invoke
    callapply        # vararg handling
    callapplylatest  # invokelatest with varargs
    # Argument tokens
    fixedargs        # use the path optimized for a fixed (small) number of arguments
    listargs         # use the general path
    # Control flow
    goto
    gotoifnot
    enter
    leave
    popexception
    returntok
end
# Some constants for handling blocks of tokens
const lastimmutable = stringptr
const lastload = loadglobalref
const laststore = storeglobalref
const firstcall = callintrinsic
const lastcall = callapplylatest

# A more stripped-down alternative to reusing TypeMapEntry for our local method
mutable struct DispatchableMethod
    next::Union{Nothing,DispatchableMethod}  # linked-list representation
    sig::Type        # for speed of matching, this is a *concrete* signature. `sig <: method.sig`
    meth::MethodInstance   # method to be called when `sig` matches
    compiled::Bool         # true if the compiled version should be called
end

mutable struct MethodListHead  # linked from the serialized code. This allows us to swap out first item on list
    next::Union{Nothing,DispatchableMethod}
end
MethodListHead() = MethodListHead(nothing)

struct SerializedCode
    scope::Union{Method,Module}
    src::CodeInfo
    ser::Vector{TokenT}
    serindex::Vector{Int}           # lookup from pc -> spc
    breakpoints::Vector{BreakpointState}
    slotnamelists::Dict{Symbol,Vector{Int}}
    generator::Bool   # true if this is for the expression-generator of a @generated function
    # The rest just prevent garbage collection of pointer-referenced items in `ser`
    # We could alternatively encode them as indexes but that seems to involve a second
    # indirection without changing their rooting, so it's not obviously advantageous.
    serargs::Vector{Vector{Any}}    # `args` stores in `ser`
    sermeth::Vector{MethodListHead} # local method tables in `ser`

    function SerializedCode(scope, src::CodeInfo; generator::Bool=false)
        serialize!(new(scope,
                       src,
                       TokenT[],
                       Int[],
                       BreakpointState[],
                       Dict{Symbol,Vector{Int}}(),
                       generator,
                       Vector{Any}[],
                       MethodListHead[]))
    end
end

mutable struct SerializedFrame
    framecode::SerializedCode
    framedata::FrameData
    pc::Int
    spc::Int
    assignment_counter::Int
    caller::Union{SerializedFrame,Nothing}
    callee::Union{SerializedFrame,Nothing}
end
SerializedFrame(framecode, framedata, pc=1, caller=nothing) =
    SerializedFrame(framecode, framedata, pc, framecode.serindex[pc], 1, caller, nothing)

function prepare_frame(framecode::SerializedCode, args::Vector{Any}, lenv::SimpleVector, caller_will_catch_err::Bool=false)
    framedata = prepare_framedata(framecode, args, caller_will_catch_err)
    resize!(framedata.sparams, length(lenv))
    # Add static parameters to environment
    for i = 1:length(lenv)
        T = lenv[i]
        isa(T, TypeVar) && continue  # only fill concrete types
        framedata.sparams[i] = T
    end
    return SerializedFrame(framecode, framedata)
end

# Functions cannot be serialized via pointer, so we use a list/Dict pair
const functionlist = []
const functionlookup = IdDict{Any,FIndexT}()   # functionlist[functionlookup[f]] == f
function function_token(@nospecialize(f))
    id = get(functionlookup, f, zero(FIndexT))
    if iszero(id)
        push!(functionlist, f)
        id = functionlookup[f] = FIndexT(length(functionlist))
    end
    return id
end

# function get_f(tok, fid)
#     fid = Int(fid)
#     if callbuiltin_n <= tok <= callbuiltin
#         return builtin[fid]
#     elseif tok == callintrinsic
#         return intrinsic[fid+1]
#     elseif calllmt <= tok <= callapplylatest
#         return functionlist[fid]
#     else
#         error("f token ", tok, " not supported")
#     end
# end

### Raw (untokenized) serialization and deserialization

# This handles basic number types. General object types are handled by their pointers.

"""
    serialize_raw!(ser::Vector{TokenT}, val)

Append a serialized version of `val` to `ser`. Returns `val`.
"""
function serialize_raw! end

"""
    val, newspc = deserialize_raw(T, ser::Vector{TokenT}, spc)

Starting at index `spc` in `ser`, deserialize a value of type `T`. Return the value `val`
and the next undeserialized index `newspc`.
"""
function deserialize_raw end

# The ::Vector{TokenT} is to prevent accidental misuse on a higher-level object
serialize_raw!(ser::Vector{TokenT}, val::TokenT) = (push!(ser, val); return val)
serialize_raw_at!(ser::Vector{TokenT}, spc, val::TokenT) = (ser[spc] = val; return spc+1)
deserialize_raw(::Type{TokenT}, ser::Vector{TokenT}, spc) = ser[spc], spc+1

if TokenT === UInt8
    split2(x::UInt16) = UInt8(x >> 8),   UInt8(x & 0x00ff)
    split2(x::UInt32) = UInt16(x >> 16), UInt16(x & 0x0000ffff)
    split2(x::UInt64) = UInt32(x >> 32), UInt32(x & 0x00000000ffffffff)
    join2(hi::UInt8,  lo::UInt8)  = UInt16(hi) << 8  + UInt16(lo)
    join2(hi::UInt16, lo::UInt16) = UInt32(hi) << 16 + UInt32(lo)
    join2(hi::UInt32, lo::UInt32) = UInt64(hi) << 32 + UInt64(lo)
    narrow(::Type{UInt16}) = UInt8
    narrow(::Type{UInt32}) = UInt16
    narrow(::Type{UInt64}) = UInt32

    function serialize_raw!(ser::Vector{TokenT}, val::Union{UInt16,UInt32,UInt64})
        hi, lo = split2(val)
        serialize_raw!(ser, hi)
        serialize_raw!(ser, lo)
        return val
    end
    function serialize_raw_at!(ser::Vector{TokenT}, spc, val::Union{UInt16,UInt32,UInt64})
        hi, lo = split2(val)
        spc = serialize_raw_at!(ser, spc, hi)
        return serialize_raw_at!(ser, spc, lo)
    end

    function deserialize_raw(::Type{T}, ser::Vector{TokenT}, spc) where T<:Union{UInt16,UInt32,UInt64}
        Thalf = narrow(T)
        hi, spc = deserialize_raw(Thalf, ser, spc)
        lo, spc = deserialize_raw(Thalf, ser, spc)
        return join2(hi, lo), spc
    end
elseif TokenT === UInt64
    serialize_raw!(ser::Vector{TokenT}, val::Union{UInt8,UInt16,UInt32}) = serialize_raw!(ser, UInt64(val))
    serialize_raw_at!(ser::Vector{TokenT}, spc, val::Union{UInt8,UInt16,UInt32}) = serialize_raw_at!(ser, spc, UInt64(val))
    function deserialize_raw(::Type{T}, ser::Vector{TokenT}, spc) where T<:Union{UInt8,UInt16,UInt32}
        val, spc = deserialize_raw(UInt, ser, spc)
        return T(val), spc
    end
end

serialize_raw!(ser::Vector{TokenT}, val::Int32)   = (serialize_raw!(ser, reinterpret(UInt32, val)); return val)
serialize_raw!(ser::Vector{TokenT}, val::Int64)   = (serialize_raw!(ser, reinterpret(UInt64, val)); return val)
serialize_raw!(ser::Vector{TokenT}, val::Float32) = (serialize_raw!(ser, reinterpret(UInt32, val)); return val)
serialize_raw!(ser::Vector{TokenT}, val::Float64) = (serialize_raw!(ser, reinterpret(UInt64, val)); return val)

serialize_raw_at!(ser::Vector{TokenT}, spc, val::Integer) = serialize_raw_at!(ser, spc, Unsigned(val))

serialize_rawptr!(ser::Vector{TokenT}, val) = (serialize_raw!(ser, reinterpret(UInt, pointer_from_objref(val))); return val)

deserialize_raw(::Type{Int32}, ser::Vector{TokenT}, spc)   = ((val, spc) = deserialize_raw(UInt32, ser, spc); return reinterpret(Int32, val), spc)
deserialize_raw(::Type{Int64}, ser::Vector{TokenT}, spc)   = ((val, spc) = deserialize_raw(UInt64, ser, spc); return reinterpret(Int64, val), spc)
deserialize_raw(::Type{Float32}, ser::Vector{TokenT}, spc) = ((val, spc) = deserialize_raw(UInt32, ser, spc); return reinterpret(Float32, val), spc)
deserialize_raw(::Type{Float64}, ser::Vector{TokenT}, spc) = ((val, spc) = deserialize_raw(UInt64, ser, spc); return reinterpret(Float64, val), spc)

function deserialize_rawptr(ser::Vector{TokenT}, spc)
    uintp, spc = deserialize_raw(UInt, ser, spc)
    return unsafe_pointer_to_objref(reinterpret(Ptr, uintp)), spc
end
function deserialize_rawptr(::Type{T}, ser::Vector{TokenT}, spc) where T
    uintp, spc = deserialize_raw(UInt, ser, spc)
    return unsafe_pointer_to_objref(reinterpret(Ptr{T}, uintp))::T, spc
end

### Serialization of code objects

# This uses tokens to convey type & other forms of meaning

function serialize!(code::SerializedCode, tok::InterpretToken)
    serialize_raw!(code.ser, eltype(code.ser)(tok))
    return code
end

function serialize!(code::SerializedCode, tok::InterpretToken, val)
    serialize!(code, tok)
    serialize_raw!(code.ser, val)
    return code
end
function serialize_ptr!(code::SerializedCode, tok::InterpretToken, val)
    serialize!(code, tok)
    serialize_rawptr!(code.ser, val)
    return code
end

function serialize_immutable!(code::SerializedCode, val)
    if val isa Int
        serialize!(code, int, val)
    elseif val isa Float32
        serialize!(code, float32, val)
    elseif val isa Float64
        serialize!(code, float64, val)
    elseif val === nothing
        serialize!(code, nothingtok)
    elseif val isa Symbol
        serialize_ptr!(code, symbolptr, val)
    elseif val isa String
        serialize_ptr!(code, stringptr, val)
    else
        error("static val type ", typeof(val), " not recognized")
    end
    return code
end

function serialize_value_load!(code::SerializedCode, val)
    if val isa GlobalRef
        val = lookup_global_ref(val)
        if isa(val, QuoteNode)
            val = val.value
        end
    end
    if val isa Core.SSAValue
        serialize!(code, loadssa, val.id)
    elseif val isa Core.SlotNumber
        serialize!(code, loadslot, val.id)
    elseif isexpr(val, :static_parameter)
        serialize!(code, loadparameter, val.args[1])
    elseif isexpr(val, :the_exception)
        serialize!(code, loadexception)
    elseif isa(val, GlobalRef)
        serialize_ptr!(code, loadglobalref, val)
    else
        serialize_immutable!(code, val)
    end
    return code
end

function serialize_call!(code::SerializedCode, stmt::Expr)
    f = stmt.args[1]
    if isa(f, GlobalRef)
        f = lookup_global_ref(f)
        if isa(f, QuoteNode)
            f = f.value
        end
    end
    if f === Base.invoke
        error("not yet implemented")
    elseif f === Base.invokelatest
        error("not yet implemented")
    elseif f === Core._apply
        error("not yet implemented")
    elseif f === Core._apply_latest
        error("not yet implemented")
    elseif f === Core._apply_pure
        error("not yet implemented")
    else
        nargs = length(stmt.args) - 1
        tok, ftok =
            if f isa Core.IntrinsicFunction
                callintrinsic, intrinsic_token(f)
            elseif f isa Core.Builtin
                callbuiltin, builtin_tokens[f]
            else
                call, function_token(f)
            end
        serialize!(code, tok)
        serialize_raw!(code.ser, FIndexT(ftok))
        if tok ∉ (callintrinsic, callbuiltin)
            mlhead = MethodListHead()
            serialize_rawptr!(code.ser, mlhead)
            push!(code.sermeth, mlhead)
        end
        if tok == callbuiltin && nargs <= max_fixed_args
            serialize!(code, fixedargs, UInt8(nargs))
        else
            args = Vector{Any}(undef, nargs)
            serialize_ptr!(code, listargs, args)
            push!(code.serargs, args)   # protect args from GC (since `code.ser` references by pointer)
        end
        for i = 1:nargs
            arg = stmt.args[i+1]
            if arg isa GlobalRef
                arg = lookup_global_ref(arg)
                if arg isa QuoteNode
                    serialize_value_load!(ser, arg.value)
                else
                    serialize_ptr!(code, loadglobalref, arg::GlobalRef)
                end
            else
                serialize_value_load!(code, arg)
            end
        end
    end
    return code
end

# main ser
function serialize!(code::SerializedCode; used=find_used(code.src))
    fixserindex = Tuple{Int,Int}[]    # gotos that need a serindex (forward-jumps, not known yet)
    for (i, stmt) in enumerate(code.src.code)
        push!(code.serindex, length(code.ser)+1)
        storeto = i ∈ used ? Core.SSAValue(i) : nothing
        if isa(stmt, Union{Int,Float64,Float32,Symbol,String,Core.SSAValue,Core.SlotNumber,GlobalRef})
            serialize_value_load!(code, stmt)
        elseif isa(stmt, Expr)
            head = stmt.head
            if head == :static_parameter
                serialize!(code, loadparameter, stmt.args[1])
            elseif head == :the_exception
                serialize!(code, loadexception)
            elseif head == :(=)
                @assert storeto === nothing
                storeto, rhs = stmt.args[1], stmt.args[2]
                if isexpr(rhs, :call)
                    serialize_call!(code, rhs)
                else
                    serialize_value_load!(code, rhs)
                end
            elseif head == :call
                serialize_call!(code, stmt)
            elseif head == :return
                serialize!(code, returntok)
                serialize_value_load!(code, stmt.args[1])
            elseif head == :gotoifnot
                serialize!(code, gotoifnot)
                serialize_value_load!(code, stmt.args[1])
                ln = stmt.args[2]
                if ln <= length(code.serindex)
                    serialize_raw!(code.ser, code.serindex[ln])
                else
                    push!(fixserindex, (length(code.ser)+1, ln))
                    serialize_raw!(code.ser, zero(eltype(code.serindex)))
                end
            else
                error("head ", head, " not yet handled")
            end
        elseif stmt isa GotoNode
            serialize!(code, goto)
            ln = stmt.label
            if ln <= length(code.serindex)
                serialize_raw!(code.ser, code.serindex[ln])
            else
                push!(fixserindex, (length(code.ser)+1, ln))
                serialize_raw!(code.ser, zero(eltype(code.serindex)))
            end
        else
            error("unhandled statement ", stmt)
        end
        if storeto != nothing
            if isa(storeto, Core.SSAValue)
                serialize!(code, storessa, storeto.id)
            elseif isa(storeto, Core.SlotNumber)
                serialize!(code, storeslot, storeto.id)
            elseif isa(storeto, GlobalRef)
                serialize_ptr!(code, storeglobalref, storeto)
            else
                error("unhandled storeto ", storeto)
            end
        end
    end
    # Now fix up the forward-jumping gotos
    for (idx, ln) in fixserindex
        serialize_raw_at!(code.ser, idx, code.serindex[ln])
    end
    return code
end

### Deserialization and executing code objects

# A full deserializer would convert back to lowered code. That's not a major priority
# right now, so we focus on other things and avoid calling it deserialization
# except where it's truly applicable.

function deserialize_token(code::SerializedCode, spc)
    itok, spc = deserialize_raw(TokenT, code.ser, spc)
    tok = InterpretToken(itok)
    return tok, spc
end
# function deserialize_ftoken(code::SerializedCode, spc)
#     return deserialize_raw(FIndexT, code.ser, spc)
# end

function deserialize_immutable(code::SerializedCode, tok, spc)
    if tok == int
        val, spc = deserialize_raw(Int, code.ser, spc)
    elseif tok == float64
        val, spc = deserialize_raw(Float64, code.ser, spc)
    elseif tok == float32
        val, spc = deserialize_raw(Float32, code.ser, spc)
    elseif tok == nothingtok
        val = nothing
    elseif tok == symbolptr
        val, spc = deserialize_rawptr(Symbol, code.ser, spc)
    elseif tok == stringptr
        val, spc = deserialize_rawptr(String, code.ser, spc)
    else
        error("immutable val type ", typeof(val), " not recognized")
    end
    return val, spc
end

function execute_load(frame, tok::InterpretToken, spc)
    code, data = frame.framecode, frame.framedata
    if tok == loadssa
        val, spc = deserialize_raw(Int, code.ser, spc)
        val = data.ssavalues[val]
    elseif tok == loadslot
        val, spc = deserialize_raw(Int, code.ser, spc)
        val = something(data.locals[val])
    elseif tok == loadparameter
        val, spc = deserialize_raw(Int, code.ser, spc)
        val = data.sparams[val]
    elseif tok == loadexception
        val = data.last_exception[]
    elseif tok == loadglobalref
        gr, spc = deserialize_rawptr(GlobalRef, code.ser, spc)
        val = getfield(gr.mod, gr.name)
    else
        return deserialize_immutable(code, tok, spc)
    end
    return val, spc
end

function execute_load(frame, spc)
    code, data = frame.framecode, frame.framedata
    tok, spc = deserialize_token(code, spc)
    return execute_load(frame, tok, spc)
end

function execute_call(frame, tok::InterpretToken, spc)
    code, data = frame.framecode, frame.framedata
    ftok, spc = deserialize_raw(FIndexT, code.ser, spc)
    if tok == callintrinsic || tok == callbuiltin
        # no methlist for intrinsics or builtins
        argtok, spc = deserialize_token(code, spc)
        if argtok == fixedargs
            @assert tok == callbuiltin
            n, spc = deserialize_raw(UInt8, code.ser, spc)
            # The advantage in fixedargs is not so much the loading but in
            # avoiding runtime dispatch in the callee. But we can also save the memory
            # of an array, so let's take advantage of that.
            if n == 0x00
                return call_builtin(ftok)
            elseif n == 0x01
                a, spc = execute_load(frame, spc)
                val = call_builtin(ftok, a)
            elseif n == 0x02
                a, spc = execute_load(frame, spc)
                b, spc = execute_load(frame, spc)
                val = call_builtin(ftok, a, b)
            elseif n == 0x03
                a, spc = execute_load(frame, spc)
                b, spc = execute_load(frame, spc)
                c, spc = execute_load(frame, spc)
                val = call_builtin(ftok, a, b, c)
            elseif n == 0x04
                a, spc = execute_load(frame, spc)
                b, spc = execute_load(frame, spc)
                c, spc = execute_load(frame, spc)
                d, spc = execute_load(frame, spc)
                val = call_builtin(ftok, a, b, c, d)
            else
                error("not implemented for ", n, " arguments")
            end
        else
            # listargs case
            args, spc = deserialize_rawptr(Vector{Any}, code.ser, spc)
            for i = 1:length(args)
                args[i], spc = execute_load(frame, spc)
            end
            if tok == callintrinsic
                f = intrinsic[ftok]
                val = ccall(:jl_f_intrinsic_call, Any, (Any, Ptr{Any}, UInt32), f, args, length(args))
            else
                val = call_builtin_listargs(ftok, args)
            end
        end
    else
        mlhead, spc = deserialize_rawptr(MethodListHead, code.ser, spc)
        argtok, spc = deserialize_token(code, spc)
        @assert argtok == listargs
        args, spc = deserialize_rawptr(Vector{Any}, code.ser, spc)
        for i = 1:length(args)
            args[i], spc = execute_load(frame, spc)
        end
        if tok == call
            # Hack: currently non-recursive
            f = functionlist[Int(ftok)]
            val = f(args...)
        else
            error("call type ", tok, " not yet implemented")
        end
    end
    return val, spc
end

function step_ser!(@nospecialize(recurse), frame, spc::Int, istoplevel::Bool)
    code, data = frame.framecode, frame.framedata
    ans = Unassigned()
    tok, spc = deserialize_token(code, spc)
    if tok <= lastload
        ans, spc = execute_load(frame, tok, spc)
    elseif firstcall <= tok <= lastcall
        ans, spc = execute_call(frame, tok, spc)
    elseif tok == goto
        newspc, _ = deserialize_raw(eltype(code.serindex), code.ser, spc)
        spc = Int(newspc)
    elseif tok == gotoifnot
        cond, spc = execute_load(frame, spc)
        newspc, spc = deserialize_raw(eltype(code.serindex), code.ser, spc)
        if !cond
            spc = Int(newspc)
        end
    elseif tok == enter
        error("unhandled")
    elseif tok == leave
        error("unhandled")
    elseif tok == popexception
        error("unhandled")
    elseif tok == returntok
        return nothing
    else
        error("unhandled token ", tok, " at ", spc, " (pc = ", pc_from_spc(frame, spc), ')')
    end
    if !isa(ans, Unassigned) && spc <= length(code.ser)
        # peek at the next token without advancing
        nexttok, spctmp = deserialize_token(code, spc)
        if nexttok == storessa
            id, spc = deserialize_raw(Int, code.ser, spctmp)
            data.ssavalues[id] = ans
        elseif nexttok == storeslot
            id, spc = deserialize_raw(Int, code.ser, spctmp)
            data.locals[id] = Some{Any}(ans)
        elseif nexttok == storeglobalref
            gr, spc = deserialize_rawptr(GlobalRef, code.ser, spctmp)
            Core.eval(gr.mod, :($(gr.name) = $(QuoteNode(ans))))
        end
    end
    return spc
end

### Printing serialized code
# Useful for debugging

print_token(io, tok::InterpretToken) = print(io, tok, ' ')

function print_load_store(io, code::SerializedCode, tok::InterpretToken, spc)
    print_token(io, tok)
    if tok <= lastimmutable
        val, spc = deserialize_immutable(code, tok, spc)
    elseif tok == loadssa || tok == storessa
        val, spc = deserialize_raw(Int, code.ser, spc)
    elseif tok == loadslot || tok == storeslot
        val, spc = deserialize_raw(Int, code.ser, spc)
    elseif tok == loadparameter
        val, spc = deserialize_raw(Int, code.ser, spc)
    elseif tok == loadexception
        val = nothing
    elseif tok == loadglobalref || tok == storeglobalref
        val, spc = deserialize_rawptr(GlobalRef, code.ser, spc)
    else
        error(tok, " is not a load")
    end
    if val !== nothing
        print(io, val, ' ')
    end
    return spc
end

function print_load(io, code::SerializedCode, spc)
    tok, spc = deserialize_token(code, spc)
    return print_load_store(io, code, tok, spc)
end

function print_call(io, code::SerializedCode, tok::InterpretToken, spc)
    print_token(io, tok)
    ftok, spc = deserialize_raw(FIndexT, code.ser, spc)
    if tok == callintrinsic
        f = intrinsic[ftok]
    elseif tok == callbuiltin
        f = builtin[ftok]
    else
        f = functionlist[ftok]
        mlhead, spc = deserialize_rawptr(MethodListHead, code.ser, spc)
    end
    show(io, f)
    print(io, ' ')
    if tok ∉ (callintrinsic, callbuiltin)
        print(io, "methlist ")
    end
    argtok, spc = deserialize_token(code, spc)
    print_token(io, argtok)
    if argtok == fixedargs
        n, spc = deserialize_raw(UInt8, code.ser, spc)
        n = Int(n)
        print(io, n, ' ')
    else
        args, spc = deserialize_rawptr(Vector{Any}, code.ser, spc)
        n = length(args)
        print(io, "<vec of length $n> ")
    end
    for i = 1:n
        spc = print_load(io, code, spc)
    end
    println()
    return spc
end

function print_serialization(io, code::SerializedCode, spc)
    tok, spc = deserialize_token(code, spc)
    if tok <= laststore
        spc = print_load_store(io, code, tok, spc)
        println()
    elseif tok <= lastcall
        spc = print_call(io, code, tok, spc)
    else
        print_token(io, tok)
        if tok == goto
            newspc, spc = deserialize_raw(eltype(code.serindex), code.ser, spc)
            println(io, newspc)
        elseif tok == gotoifnot
            spc = print_load(io, code, spc)
            newspc, spc = deserialize_raw(eltype(code.serindex), code.ser, spc)
            println(io, newspc)
        elseif tok == enter
            error("unhandled")
        elseif tok == leave
            error("unhandled")
        elseif tok == popexception
            error("unhandled")
        elseif tok == returntok
            spc = print_load(io, code, spc)
            println()
        else
            error("unhandled token ", tok, " at ", spc)
        end
    end
    return spc
end

function print_serialization(io, code::SerializedCode)
    nd = ndigits(length(code.ser))
    spc = 1
    while spc <= length(code.ser)
        print(io, lpad(spc, nd), ": ")
        spc = print_serialization(io, code, spc)
    end
    return nothing
end

#     print(io, tok, ' ')
#     if tok == assignprev
#         lhsid, spc = ser[spc], spc+1
#         if f isa Core.IntrinsicFunction
#         println(io, lhsid)
#     elseif tok ∈ (storessa, assignslot)
#         lhsid, spc = ser[spc], spc+1
#         print(io, tok == storessa ? " %" : " @", Int(lhsid), ' ')
#         spc = print_serialization(io, mod, ser, spc)
#     elseif tok == assignglobalref
#         lhs, spc = deserialize_globalref(ser, spc)
#         rhsid, spc = ser[spc], spc+1
#         print(io, lhs, ' ')
#         spc = print_serialization(io, mod, ser, spc)
#     elseif tok == assignmodsym
#         lhs, spc = deserialize_symbol(ser, spc)
#         rhsid, spc = ser[spc], spc+1
#         println(io, mod, '.', lhs, ' ')
#         spc = print_serialization(io, mod, ser, spc)
#     elseif tok == callbuiltin_n
#         f, spc = get_f(tok, ser[spc]), spc+1
#         n, spc = Int(ser[spc]), spc+1
#         spc = print_call(io, f, n, mod, ser, spc)
#     elseif callbuiltin <= tok <= callapplylatest
#         f, spc = get_f(tok, ser[spc]), spc+1
#         args, spc = vecany(ser[spc]), spc+1
#         spc = print_call(io, f, length(args), mod, ser, spc)
#     elseif tok == goto
#         idx, spc = Int(ser[spc]), spc+1
#         println(io, "goto ", idx)
#     elseif tok == gotoifnot
#         condtok, spc = ser[spc], spc+1
#         if condtok ∈ (loadssa, loadslot)
#             id, spc = Int(ser[spc]), spc+1
#         else
#             error("unsupported conditional token ", tok)
#         end
#         idx, spc = Int(ser[spc]), spc+1
#         println(io, "gotoifnot ", condtok == loadssa ? " %" : " @", id, ' ', idx)
#     elseif tok == enter
#         error("unhandled")
#     elseif tok == leave
#         error("unhandled")
#     elseif tok == popexception
#         error("unhandled")
#     elseif tok == returntok
#         datatok, spc = ser[spc], spc+1
#         if datatok ∈ (loadssa, loadslot)
#             id, spc = Int(ser[spc]), spc+1
#         else
#             error("unsupported conditional token ", tok)
#         end
#         idx, spc = Int(ser[spc]), spc+1
#         println(io, "return ", datatok == loadssa ? " %" : " @", id, ' ', idx)
#     else
#         error("unhandled token ", tok, " at ", spc)
#     end
#     return spc
# end
#
# function print_serialization(io, mod, ser)
#     n = length(ser)
#     nd = ndigits(n)
#     spc = 1
#     while spc <= n
#         print(io, lpad(string(spc), nd), ": ")
#         spc = print_serialization(io, mod, ser, spc)
#     end
# end
#
#
# ### interpreting serialized code
#
# function step_ser!(@nospecialize(recurse), frame, spc::Int, istoplevel::Bool)
#     code, data = frame.framecode, frame.framedata
#     ser = code.ser
#     tok, spc = InterpretToken(ser[spc]), spc+1
#     if tok < call_builtin_n
#         ret, spc = deserialize_value(ser, spc, data)
#         spc = assign_value!(data, code, spc, ret)
#     if tok == callbuiltin_n
#         f, spc = ser[spc], spc+1
#         n, spc = Int(ser[spc]), spc+1
#         ret, spc = call_builtin_n(ftok, n, ser, spc, data)
#         spc = assign_value!(data, code, spc, ret)
#     elseif tok == callbuiltin
#         ftok, spc = ser[spc], spc+1
#         args, spc = vecany(ser[spc]), spc+1
#         for i = 1:length(args)
#             args[i], spc = deserialize_value(ser, spc, data)
#         end
#         ret = call_builtin(fflag, args)
#         spc = assign_value!(data, spc, ret)
#     elseif tok == callintrinsic
#         f, spc = intrinsic[ser[spc]], spc+1
#         args, spc = vecany(ser[spc]), spc+1
#         for i = 1:length(args)
#             args[i], spc = deserialize_value(ser, spc, data)
#         end
#         ret = ccall(:jl_f_intrinsic_call, Any, (Any, Ptr{Any}, UInt32), f, args, length(args))
#         spc = assign_value!(data, spc, ret)
#     elseif tok == calllmt
#         f, spc = functionlist[Int(ser[spc])], spc+1
#         fargs, spc = vecany(ser[spc]), spc+1
#         fargs[1] = f
#         for i = 1:length(args)
#             fargs[i+1], spc = deserialize_value(ser, spc, data)
#         end
#         calleecode, lenv = get_call_framecode(fargs, calleecode, spc)
#         if lenv === nothing
#             if isa(calleecode, Compiled)
#                 return Base.invokelatest(fargs...)
#             end
#             return framecode  # this was a Builtin
#         end
#         # TODO: "inline" trivial frames
#         newframe = prepare_frame_caller(frame, calleecode, fargs, lenv)
#         if recurse === finish_and_return!
#             # Optimize this case to avoid dynamic dispatch
#             ret = finish_and_return!(finish_and_return!, newframe, false)
#         else
#             ret = recurse(recurse, newframe, false)
#         end
#         spc = assign_value!(data, spc, ret)
#     elseif tok == calldispatch
#         f, spc = obj(ser[spc]), spc+1
#         args, spc = vecany(ser[spc]), spc+1
#         for i = 1:length(args)
#             args[i], spc = deserialize_value(ser, spc, data)
#         end
#         ret = f(args...)
#         spc = assign_value!(data, spc, ret)
#     elseif tok == calllatest
#         error("unhandled")
#     elseif tok == callinvoke
#         error("unhandled")
#     elseif tok == callapply
#         error("unhandled")
#     elseif tok == callapplylatest
#         error("unhandled")
#     elseif tok == goto
#         spc = Int(ser[spc])
#     elseif tok == gotoifnot
#         cond, spc = deserialize_value(ser, spc, data)
#         if !cond
#             spc = Int(ser[spc])
#         end
#     elseif tok == enter
#         error("unhandled")
#     elseif tok == leave
#         error("unhandled")
#     elseif tok == popexception
#         error("unhandled")
#     elseif tok == returntok
#         return nothing
#     else
#         error("unhandled token ", tok, " at ", spc, " (pc = ", pc_from_spc(frame, spc), ')')
#     end
# end

function __init__()
    fill_ftables()
end

end
