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
        breakpoint!(framecode, stmtidx, bp.condition, bp.enabled[])
        push!(bp.applications, BreakpointRef(framecode, stmtidx))
    end
end

function framecode_matches_breakpoint(framecode::FrameCode, bp::BreakpointSignature)
    function extract_function_from_method(m::Method)
        sig = Base.unwrap_unionall(m.sig)
        ft0 = sig.parameters[1]
        ft = Base.unwrap_unionall(ft0)
        if ft <: Function && isa(ft, DataType) && isdefined(ft, :instance)
            return ft.instance
        elseif isa(ft, DataType) && ft.name === Type.body.name
            f = ft.parameters[1]
        else
            return ft
        end
    end

    framecode.scope isa Method || return false
    meth = framecode.scope
    bp.f isa Method && return meth === bp.f
    bp.f === extract_function_from_method(meth) || return false
    bp.sig === nothing && return true
    return bp.sig <: meth.sig
end

"""
    breakpoint(f, [sig], [line], [condition])

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
    bp = BreakpointSignature(f, sig, line, condition, Ref(true), BreakpointRef[])
    add_to_existing_framecodes(bp)
    idx = findfirst(bp2 -> same_location(bp, bp2), _breakpoints)
    idx === nothing ? push!(_breakpoints, bp) : (_breakpoints[idx] = bp)
    return bp
end
breakpoint(f, sig, condition::Condition) = breakpoint(f, sig, 0, condition)
breakpoint(f, line::Integer, condition::Condition=nothing) = breakpoint(f, nothing, line, condition)
breakpoint(f, condition::Condition) = breakpoint(f, nothing, 0, condition)


"""
    breakpoint(file, line, [condition])

Set a breakpoint in `file` at `line`. The argument `file` can be a filename, a partial path or absolute path.
For example, `file = foo.jl` will match against all files with the name `foo.jl`,
`file = src/foo.jl` will match against all paths containing `src/foo.jl`, e.g. both `Foo/src/foo.jl` and `Bar/src/foo.jl`.
Absolute paths only matches against the file with that exact absolute path.
"""
function breakpoint(file::String, line::Integer, condition::Condition=nothing)
    file = normpath(file)
    bp = BreakpointFileLocation(file, CodeTracking.maybe_fix_path(abspath(file)), line, condition, Ref(true), BreakpointRef[])
    add_to_existing_framecodes(bp)
    idx = findfirst(bp2 -> same_location(bp, bp2), _breakpoints)
    idx === nothing ? push!(_breakpoints, bp) : (_breakpoints[idx] = bp)
    return bp
end

function framecode_matches_breakpoint(framecode::FrameCode, bp::BreakpointFileLocation)
    framecode.scope isa Method || return false
    meth = framecode.scope
    methpath = CodeTracking.maybe_fix_path(String(meth.file))
    if bp.abspath == methpath || endswith(methpath, bp.path)
        return method_contains_line(meth, bp.line)
    else
        return false
    end
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
function breakpoint!(framecode::FrameCode, pc, condition::Condition=nothing, enabled=true)
    stmtidx = pc
    if condition === nothing
        framecode.breakpoints[stmtidx] = BreakpointState(enabled)
    else
        mod, cond = _unpack(condition)
        fex = prepare_slotfunction(framecode, cond)
        framecode.breakpoints[stmtidx] = BreakpointState(enabled, Core.eval(mod, fex))
    end
end
breakpoint!(frame::Frame, pc=frame.pc, condition::Condition=nothing) =
    breakpoint!(frame.framecode, pc, condition)

update_states!(bp::AbstractBreakpoint) = foreach(bpref -> update_state!(bpref, bp.enabled[]), bp.applications)
update_state!(bp::BreakpointRef, v::Bool) = bp[] = v

"""
    enable(bp::AbstractBreakpoint)

Enable breakpoint `bp`.
"""
enable(bp::AbstractBreakpoint) = (bp.enabled[] = true; update_states!(bp))
enable(bp::BreakpointRef) = bp[] = true


"""
    disable(bp::AbstractBreakpoint)

Disable breakpoint `bp`. Disabled breakpoints can be re-enabled with [`enable`](@ref).
"""
disable(bp::AbstractBreakpoint) = (bp.enabled[] = false; update_states!(bp))
disable(bp::BreakpointRef) = bp[] = false

"""
    remove(bp::AbstractBreakpoint)

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
toggle(bp::AbstractBreakpoint) = (bp.enabled[] = !bp.enabled[]; update_states!(bp))
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
        foreach(remove, bp.applications)
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
