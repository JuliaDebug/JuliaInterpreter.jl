"""
`Compiled` is a trait indicating that any `:call` expressions should be evaluated
using Julia's normal compiled-code evaluation. The alternative is to pass `stack=Frame[]`,
which will cause all calls to be evaluated via the interpreter.
"""
struct Compiled end
Base.similar(::Compiled, sz) = Compiled()  # to support similar(stack, 0)

# A type used transiently in renumbering CodeInfo SSAValues (to distinguish a new SSAValue from an old one)
struct NewSSAValue
    id::Int
end

# Breakpoint support
truecondition(frame) = true
falsecondition(frame) = false
const break_on_error = Ref(false)
const break_on_throw = Ref(false)

"""
    BreakpointState(isactive=true, condition=JuliaInterpreter.truecondition)

`BreakpointState` represents a breakpoint at a particular statement in
a `FrameCode`. `isactive` indicates whether the breakpoint is currently
[`enable`](@ref)d or [`disable`](@ref)d. `condition` is a function that accepts
a single `Frame`, and `condition(frame)` must return either
`true` or `false`. Execution will stop at a breakpoint only if `isactive`
and `condition(frame)` both evaluate as `true`. The default `condition` always
returns `true`.

To create these objects, see [`breakpoint`](@ref).
"""
struct BreakpointState
    isactive::Bool
    condition::Function
end
BreakpointState(isactive::Bool) = BreakpointState(isactive, truecondition)
BreakpointState() = BreakpointState(true)

function breakpointchar(bps::BreakpointState)
    if bps.isactive
        return bps.condition === truecondition ? 'b' : 'c'  # unconditional : conditional
    end
    return bps.condition === falsecondition ? ' ' : 'd'     # no breakpoint : disabled
end

"""
`FrameCode` holds static information about a method or toplevel code.
One `FrameCode` can be shared by many calling `Frame`s.

Important fields:
- `scope`: the `Method` or `Module` in which this frame is to be evaluated
- `src`: the `CodeInfo` object storing (optimized) lowered source code
- `methodtables`: a vector, each entry potentially stores a "local method table" for the corresponding
  `:call` expression in `src` (undefined entries correspond to statements that do not
  contain `:call` expressions)
- `used`: a `BitSet` storing the list of SSAValues that get referenced by later statements.
"""
struct FrameCode
    scope::Union{Method,Module}
    src::CodeInfo
    methodtables::Vector{Union{Compiled,TypeMapEntry}} # line-by-line method tables for generic-function :call Exprs
    breakpoints::Vector{BreakpointState}
    used::BitSet
    generator::Bool   # true if this is for the expression-generator of a @generated function
end

const BREAKPOINT_EXPR = :($(QuoteNode(getproperty))($JuliaInterpreter, :__BREAKPOINT_MARKER__))
function FrameCode(scope, src::CodeInfo; generator=false, optimize=true)
    if optimize
        src, methodtables = optimize!(copy_codeinfo(src), scope)
    else
        src = copy_codeinfo(src)
        methodtables = Vector{Union{Compiled,TypeMapEntry}}(undef, length(src.code))
    end
    breakpoints = Vector{BreakpointState}(undef, length(src.code))
    for (i, pc_expr) in enumerate(src.code)
        if pc_expr == BREAKPOINT_EXPR
            breakpoints[i] = BreakpointState()
            src.code[i] = nothing
        end
    end
    used = find_used(src)
    return FrameCode(scope, src, methodtables, breakpoints, used, generator)
end

nstatements(framecode::FrameCode) = length(framecode.src.code)

Base.show(io::IO, framecode::FrameCode) = print_framecode(io, framecode)

"""
`FrameInstance` represents a method specialized for particular argument types.

Fields:
- `framecode`: the [`FrameCode`](@ref) for the method
- `sparam_vals`: the static parameter values for the method
"""
struct FrameInstance
    framecode::FrameCode
    sparam_vals::SimpleVector
    enter_generated::Bool
end

Base.show(io::IO, instance::FrameInstance) =
    print(io, "FrameInstance(", scopeof(instance.framecode), ", ", instance.sparam_vals, ", ", instance.enter_generated, ')')

"""
`FrameData` holds the arguments, local variables, and intermediate execution state
in a particular call frame.

Important fields:
- `locals`: a vector containing the input arguments and named local variables for this frame.
  The indexing corresponds to the names in the `slotnames` of the src. Use [`locals`](@ref)
  to extract the current value of local variables.
- `ssavalues`: a vector containing the
  [Static Single Assignment](https://en.wikipedia.org/wiki/Static_single_assignment_form)
  values produced at the current state of execution
- `sparams`: the static type parameters, e.g., for `f(x::Vector{T}) where T` this would store
  the value of `T` given the particular input `x`.
- `exception_frames`: a list of indexes to `catch` blocks for handling exceptions within
  the current frame. The active handler is the last one on the list.
- `last_exception`: the exception `throw`n by this frame or one of its callees.
"""
struct FrameData
    locals::Vector{Union{Nothing,Some{Any}}}
    ssavalues::Vector{Any}
    sparams::Vector{Any}
    exception_frames::Vector{Int}
    last_exception::Base.RefValue{Any}
    caller_will_catch_err::Bool
    # A vector from names to the slotnumber of that name
    # for which a reference was last encountered.
    last_reference::Dict{Symbol,Int}
    callargs::Vector{Any}  # a temporary for processing arguments of :call exprs
end


"""
`Frame` represents the current execution state in a particular call frame.
Fields:
- `framecode`: the [`FrameCode`] for this frame
- `framedata`: the [`FrameData`] for this frame
- `pc`: the program counter (integer index of the next statment to be evaluated) for this frame
- `caller`: the parent caller of this frame, or `nothing`
- `callee`: the frame called by this one, or `nothing`

The `Base` functions `show_backtrace` and `display_error` are overloaded such that
`show_backtrace(io::IO, frame::Frame)` and `display_error(io::IO, er, frame::Frame)`
shows a backtrace or error, respectively, in a similar way as to how Base shows
them.
"""
mutable struct Frame
    framecode::FrameCode
    framedata::FrameData
    pc::Int
    caller::Union{Frame,Nothing}
    callee::Union{Frame,Nothing}
end
Frame(framecode, framedata, pc=1, caller=nothing) = Frame(framecode, framedata, pc, caller, nothing)

caller(frame) = frame.caller
callee(frame) = frame.callee

function traverse(f, frame)
    while f(frame) !== nothing
        frame = f(frame)
    end
    return frame
end

"""
    rframe = root(frame)

Return the initial frame in the call stack.
"""
root(frame) = traverse(caller, frame)

"""
    lframe = leaf(frame)

Return the deepest callee in the call stack.
"""
leaf(frame) = traverse(callee, frame)

function Base.show(io::IO, frame::Frame)
    frame_loc = CodeTracking.replace_buildbot_stdlibpath(repr(scopeof(frame)))
    println(io, "Frame for ", frame_loc)
    pc = frame.pc
    ns = nstatements(frame.framecode)
    range = get(io, :limit, false) ? (max(1, pc-2):min(ns, pc+2)) : (1:ns)
    first(range) > 1 && println(io, "⋮")
    print_framecode(io, frame.framecode; pc=pc, range=range)
    last(range) < ns && print(io, "\n⋮")
    print_vars(IOContext(io, :limit=>true, :compact=>true), locals(frame))
    if caller(frame) !== nothing
        print(io, "\ncaller: ", scopeof(caller(frame)))
    end
    if callee(frame) !== nothing
        print(io, "\ncallee: ", scopeof(callee(frame)))
    end
end

"""
`Variable` is a struct representing a variable with an asigned value.
By calling the function `locals`[@ref] on a `Frame`[@ref] a
`Vector` of `Variable`'s is returned.

Important fields:
- `value::Any`: the value of the local variable
- `name::Symbol`: the name of the variable as given in the source code
- `isparam::Bool`: if the variable is a type parameter, for example `T` in `f(x::T) where {T} = x` .
"""
struct Variable
    value::Any
    name::Symbol
    isparam::Bool
end
Base.show(io::IO, var::Variable) = (print(io, var.name, " = "); show(io,var.value))
Base.isequal(var1::Variable, var2::Variable) =
    var1.value == var2.value && var1.name == var2.name && var1.isparam == var2.isparam

# A type that is unique to this package for which there are no valid operations
struct Unassigned end

"""
    BreakpointRef(framecode, stmtidx)
    BreakpointRef(framecode, stmtidx, err)

A reference to a breakpoint at a particular statement index `stmtidx` in `framecode`.
If the break was due to an error, supply that as well.

Commands that execute complex control-flow (e.g., `next_line!`) may also return a
`BreakpointRef` to indicate that the execution stack switched frames, even when no
breakpoint has been set at the corresponding statement.
"""
struct BreakpointRef
    framecode::FrameCode
    stmtidx::Int
    err
end
BreakpointRef(framecode, stmtidx) = BreakpointRef(framecode, stmtidx, nothing)

function Base.show(io::IO, bp::BreakpointRef)
    if checkbounds(Bool, bp.framecode.breakpoints, bp.stmtidx)
        lineno = linenumber(bp.framecode, bp.stmtidx)
        print(io, "breakpoint(", bp.framecode.scope, ", line ", lineno)
    else
        print(io, "breakpoint(", bp.framecode.scope, ", %", bp.stmtidx)
    end
    if bp.err !== nothing
        print(io, ", ", bp.err)
    end
    print(io, ')')
end
