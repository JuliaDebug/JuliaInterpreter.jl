const Condition = Union{Nothing,Expr,Tuple{Module,Expr}}
abstract type AbstractBreakpoint end


const _breakpoints = AbstractBreakpoint[]
breakpoints() = copy(_breakpoints)

function add_to_existing_framecodes(bp::AbstractBreakpoint)
    for framecode in values(framedict)
        add_breakpoint_if_match!(framecode, bp)
    end
end

function add_breakpoint_if_match!(framecode::FrameCode, bp::AbstractBreakpoint)
    if framecode_matches_breakpoint(framecode, bp)
        stmtidx = bp.line === 0 ? 1 : statementnumber(framecode, bp.line)
        bpref = breakpoint!(framecode, stmtidx, bp.condition)
        push!(bp.applications, BreakpointRef(framecode, stmtidx))
    end
end

struct BreakpointSignature <: AbstractBreakpoint
    f # Method or function
    sig::Union{Nothing, Type}
    line::Int # 0 is a sentinel for first statement
    condition::Condition
    applications::Vector{BreakpointRef}
end
Base.isequal(bp2::BreakpointSignature, bp::BreakpointSignature) = 
    bp2.f == bp.f && bp2.sig == bp.sig && bp2.line == bp.line && bp.condition == bp2.condition
function Base.show(io::IO, bp::BreakpointSignature)
    print(io, "BreakpointSignature: ")
    print(io, bp.f)
    if bp.sig !== nothing
        print(io, '(', join("::" .* string.(bp.sig.types), ", "), ')')
    end
    if bp.line !== 0
        print(io, bp.line)
    end
    if bp.condition !== nothing
        print(io, " ", bp.condition)
    end
end

function extract_function_from_method(m::Method)
    sig = m.sig
    while sig isa UnionAll
        sig = sig.body
    end
    f = sig.parameters[1]
    if isdefined(f, :instance)
        return f.instance
    else
        return f
    end
end

function framecode_matches_breakpoint(framecode::FrameCode, bp::BreakpointSignature)
    framecode.scope isa Method || return false
    meth = framecode.scope
    bp.f isa Method && return meth === bp.f
    bp.f === extract_function_from_method(meth) || return false
    bp.sig === nothing && return true
    tt = Base.signature_type(bp.f, bp.sig)
    return whichtt(tt) === meth
end

"""
    breakpoint(f)
    breakpoint(f, line)
    breakpoint(f, condition)
    breakpoint(f, line, condition)
    breakpoint(f, sig)
    breakpoint(f, sig, line)
    breakpoint(f, sig, condition)
    breakpoint(f, sig, line, condition)


Add a breakpoint to `f` with the specified argument types `sig`.¨
If `sig` is not given, the breakpoint will apply to all methods of `f`.
If `f` is a method, the breakpoint will only apply to that method.
Optionally specify an absolute line number `line` in the source file; the default
is to break upon entry at the first line of the body.
Without `condition`, the breakpoint will be triggered every time it is encountered;
the second only if `condition` evaluates to `true`.
`condition` should be written in terms of the arguments and local variables of `f`.

# Example
```julia
function radius2(x, y)
    return x^2 + y^2
end

breakpoint(radius2, Tuple{Int,Int}, :(y > x))
```
"""
function breakpoint(f, sig=nothing, line::Integer=0, condition::Condition=nothing)
    sig !== nothing && (sig = Base.to_tuple_type(sig))
    bp = BreakpointSignature(f, sig, line, condition, BreakpointRef[])
    add_to_existing_framecodes(bp)
    if !any(isequal(bp), _breakpoints)
        push!(_breakpoints, bp)
    end
    return bp
end
breakpoint(f, sig, condition::Condition) = breakpoint(f, sig, 0, condition)
breakpoint(f, line::Integer, condition::Condition=nothing) = breakpoint(f, nothing, line, condition)
breakpoint(f, condition::Condition) = breakpoint(f, nothing, 0, condition)

struct BreakpointFileLocation <: AbstractBreakpoint
    file::Symbol
    line::Integer
    condition::Condition
    applications::Vector{BreakpointRef}
end
Base.isequal(bp2::BreakpointFileLocation, bp::BreakpointFileLocation) = 
    bp2.file == bp.line && bp2.line == bp.line && bp2.condition == bp.condition
function Base.show(io::IO, bp::BreakpointFileLocation)
    print(io, "BreakpointFileLocation: ")
    print(io, bp.file, ':', bp.line)
    if bp.condition !== nothing
        print(io, bp.condition)
    end
end

"""
    breakpoint(filename, line)
    breakpoint(filename, line, condition)

Set a breakpoint at the specified file and line number.
"""
function breakpoint(file::String, line::Integer=0, condition::Condition=nothing)
    maybe_source_file = Base.find_source_file(file)
    if maybe_source_file === nothing
        file = abspath(file)
    end
    bp = BreakpointFileLocation(Symbol(file), line, condition, BreakpointRef[])
    add_to_existing_framecodes(bp)
    if !any(isequal(bp), _breakpoints)
        push!(_breakpoints, bp)
    end
    return bp
end

function framecode_matches_breakpoint(framecode::FrameCode, bp::BreakpointFileLocation)
    framecode.scope isa Method || return false
    meth = framecode.scope
    meth.file == bp.file || return false
    return method_contains_line(meth, bp.line)
end

Base.getindex(bp::BreakpointRef) = bp.framecode.breakpoints[bp.stmtidx]
function Base.setindex!(bp::BreakpointRef, isactive::Bool)
    bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(isactive, bp[].condition)
end

function shouldbreak(frame::Frame, pc::Int)
    bps = frame.framecode.breakpoints
    isassigned(bps, pc) || return false
    bp = bps[pc]
    bp.isactive || return false
    return Base.invokelatest(bp.condition, frame)::Bool
end

function prepare_slotfunction(framecode::FrameCode, body::Union{Symbol,Expr})
    ismeth = framecode.scope isa Method
    uslotnames = Set{Symbol}()
    slotnames  = Symbol[]
    for name in framecode.src.slotnames
        if name ∉ uslotnames
            push!(slotnames, name)
            push!(uslotnames, name)
        end
    end
    framename, dataname = gensym("frame"), gensym("data")
    assignments = Expr[:($dataname = $framename.framedata)]
    default = Unassigned()
    for i = 1:length(slotnames)
        slotname = framecode.src.slotnames[i]
        qslotname = QuoteNode(slotname)
        getexpr = :(something($dataname.locals[$dataname.last_reference[$qslotname]]))
        push!(assignments, Expr(:(=), slotname, :(haskey($dataname.last_reference, $qslotname) ? $getexpr : $default)))
    end
    if ismeth
        syms = sparam_syms(framecode.scope)
        for i = 1:length(syms)
            push!(assignments, Expr(:(=), syms[i], :($dataname.sparams[$i])))
        end
    end
    funcname = ismeth ? gensym("slotfunction") : gensym(Symbol(framecode.scope.name, "_slotfunction"))
    return Expr(:function, Expr(:call, funcname, framename), Expr(:block, assignments..., body))
end

_unpack(condition) = isa(condition, Expr) ? (Main, condition) : condition

## The fundamental implementations of breakpoint-setting
function breakpoint!(framecode::FrameCode, pc, condition::Condition=nothing)
    stmtidx = pc
    if condition === nothing
        framecode.breakpoints[stmtidx] = BreakpointState()
    else
        mod, cond = _unpack(condition)
        fex = prepare_slotfunction(framecode, cond)
        framecode.breakpoints[stmtidx] = BreakpointState(true, Core.eval(mod, fex))
    end
end
breakpoint!(frame::Frame, pc=frame.pc, condition::Condition=nothing) =
    breakpoint!(frame.framecode, pc, condition)

"""
    enable(bp::AbstractBreakpoint)

Enable breakpoint `bp`.
"""
enable(bp::AbstractBreakpoint) = foreach(enable, bp.applications)
enable(bp::BreakpointRef)  = bp[] = true


"""
    disable(bp::BreakpointRef)

Disable breakpoint `bp`. Disabled breakpoints can be re-enabled with [`enable`](@ref).
"""
disable(bp::AbstractBreakpoint) = foreach(disable, bp.applications)
disable(bp::BreakpointRef)  = bp[] = false


"""
    remove(bp::BreakpointRef)

Remove (delete) breakpoint `bp`. Removed breakpoints cannot be re-enabled.
"""
function remove(bp::AbstractBreakpoint)
    idx = findfirst(isequal(bp), _breakpoints)
    idx === nothing || deleteat!(_breakpoints, idx)
    foreach(remove, bp.applications)
end
function remove(bp::BreakpointRef)
    bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(false, falsecondition)
    return nothing
end

"""
    toggle(bp::AbstractBreakpoint)

Toggle breakpoint `bp`.
"""
toggle(bp::AbstractBreakpoint) = foreach(toggle, bp.applications)
function toggle(bp::BreakpointRef)
    state = bp[]
    bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(!state.isactive, state.condition)
end

"""
    enable()

Enable all breakpoints.
"""
enable() = foreach(enable, _breakpoints)

"""
    disable()

Disable all breakpoints.
"""
disable() = foreach(disable, _breakpoints)

"""
    remove()

Remove all breakpoints.
"""
function remove()
    for bp in _breakpoints
        remove(bp)
    end
    empty!(_breakpoints)
end

"""
    break_on(states...)

Turn on automatic breakpoints when any of the conditions described in `states` occurs.
The supported states are:

- `:error`: trigger a breakpoint any time an uncaught exception is thrown
- `:throw` : trigger a breakpoint any time a throw is executed (even if it will eventually be caught)
"""
function break_on(states::Vararg{Symbol})
    for state in states
        if state == :error
            break_on_error[] = true
        elseif state == :throw
            break_on_throw[] = true
        else
            throw(ArgumentError(string("unsupported state :", state)))
        end
    end
end

"""
    break_off(states...)

Turn off automatic breakpoints when any of the conditions described in `states` occurs.
See [`break_on`](@ref) for a description of valid states.
"""
function break_off(states::Vararg{Symbol})
    for state in states
        if state == :error
            break_on_error[] = false
        elseif state == :throw
            break_on_throw[] = false
        else
            throw(ArgumentError(string("unsupported state :", state)))
        end
    end
end


"""
    @breakpoint f(args...) condition=nothing
    @breakpoint f(args...) line condition=nothing

Break upon entry, or at the specified line number, in the method called by `f(args...)`.
Optionally supply a condition expressed in terms of the arguments and internal variables
of the method.
If `line` is supplied, it must be a literal integer.

# Example

Suppose a method `mysum` is defined as follows, where the numbers to the left are the line
number in the file:

```
12 function mysum(A)
13     s = zero(eltype(A))
14     for a in A
15         s += a
16     end
17     return s
18 end
```

Then

```
@breakpoint mysum(A) 15 s>10
```

would cause execution of the loop to break whenever `s>10`.
"""
macro breakpoint(call_expr, args...)
    whichexpr = InteractiveUtils.gen_call_with_extracted_types(__module__, :which, call_expr)
    haveline, line, condition = false, 0, nothing
    while !isempty(args)
        arg = first(args)
        if isa(arg, Integer)
            haveline, line = true, arg
        else
            condition = arg
        end
        args = Base.tail(args)
    end
    condexpr = condition === nothing ? nothing : Expr(:quote, condition)
    if haveline
        return quote
            local method = $whichexpr
            $breakpoint(method, $line, $condexpr)
        end
    else
        return quote
            local method = $whichexpr
            $breakpoint(method, $condexpr)
        end
    end
end

const __BREAKPOINT_MARKER__ = nothing

"""
    @bp

Insert a breakpoint at a location in the source code.
"""
macro bp()
    return esc(:($(JuliaInterpreter).__BREAKPOINT_MARKER__))
end
