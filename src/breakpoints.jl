const _breakpoints = BreakpointRef[]
breakpoints() = copy(_breakpoints)

Base.getindex(bp::BreakpointRef) = bp.framecode.breakpoints[bp.stmtidx]
function Base.setindex!(bp::BreakpointRef, isactive::Bool)
    bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(isactive, bp[].condition)
end
function toggle!(bp::BreakpointRef)
    state = bp[]
    bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(!state.isactive, state.condition)
end

function add_breakpoint(framecode, stmtidx)
    bp = BreakpointRef(framecode, stmtidx)
    # Since there can be only one BreakpointState for a given framecode/stmtidx,
    # check whether _breakpoints is already storing a reference to that location
    idx = findfirst(isequal(bp), _breakpoints)
    if idx === nothing
        push!(_breakpoints, bp)
    end
    return bp
end

function shouldbreak(frame, pc=frame.pc[])
    idx = convert(Int, pc)
    isassigned(frame.code.breakpoints, idx) || return false
    bp = frame.code.breakpoints[idx]
    bp.isactive || return false
    return Base.invokelatest(bp.condition, frame)::Bool
end

function prepare_slotfunction(framecode::JuliaFrameCode, body::Union{Symbol,Expr})
    ismeth = framecode.scope isa Method
    uslotnames = Set{Symbol}()
    slotnames  = Symbol[]
    for name in framecode.code.slotnames
        if name ∉ uslotnames
            push!(slotnames, name)
            push!(uslotnames, name)
        end
    end
    assignments = Expr[]
    framename = gensym("frame")
    default = Unassigned()
    for i = 1:length(slotnames)
        slotname = framecode.code.slotnames[i]
        qslotname = QuoteNode(slotname)
        getexpr = :(something($framename.locals[$framename.last_reference[$qslotname]]))
        push!(assignments, Expr(:(=), slotname, :(haskey($framename.last_reference, $qslotname) ? $getexpr : $default)))
    end
    if ismeth
        syms = sparam_syms(framecode.scope)
        for i = 1:length(syms)
            push!(assignments, Expr(:(=), syms[i], :($framename.sparams[$i])))
        end
    end
    funcname = ismeth ? gensym("slotfunction") : gensym(Symbol(framecode.scope.name, "_slotfunction"))
    return Expr(:function, Expr(:call, funcname, framename), Expr(:block, assignments..., body))
end

const Condition = Union{Nothing,Expr,Tuple{Module,Expr}}
_unpack(condition) = isa(condition, Expr) ? (Main, condition) : condition

## The fundamental implementations of breakpoint-setting
function breakpoint!(framecode::JuliaFrameCode, pc, condition::Condition=nothing)
    stmtidx = convert(Int, pc)
    if condition === nothing
        framecode.breakpoints[stmtidx] = BreakpointState()
    else
        mod, cond = _unpack(condition)
        fex = prepare_slotfunction(framecode, cond)
        framecode.breakpoints[stmtidx] = BreakpointState(true, Core.eval(mod, fex))
    end
    return add_breakpoint(framecode, stmtidx)
end
breakpoint!(frame::JuliaStackFrame, pc=frame.pc[], condition::Condition=nothing) =
    breakpoint!(frame.code, pc, condition)

"""
    enable(bp::BreakpointRef)

Enable breakpoint `bp`.
"""
enable(bp::BreakpointRef)  = bp[] = true

"""
    disable(bp::BreakpointRef)

Disable breakpoint `bp`. Disabled breakpoints can be re-enabled with [`enable`](@ref).
"""
disable(bp::BreakpointRef) = bp[] = false

"""
    remove(bp::BreakpointRef)

Remove (delete) breakpoint `bp`. Removed breakpoints cannot be re-enabled.
"""
function remove(bp::BreakpointRef)
    idx = findfirst(isequal(bp), _breakpoints)
    deleteat!(_breakpoints, idx)
    bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(false, falsecondition)
    return nothing
end

"""
    enable()

Enable all breakpoints.
"""
enable() = for bp in _breakpoints enable(bp) end

"""
    disable()

Disable all breakpoints.
"""
disable() = for bp in _breakpoints disable(bp) end

"""
    remove()

Remove all breakpoints.
"""
function remove()
    for bp in _breakpoints
        bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(false, falsecondition)
    end
    empty!(_breakpoints)
    return nothing
end

"""
    breakpoint(f, sig)
    breakpoint(f, sig, line)
    breakpoint(f, sig, condition)
    breakpoint(f, sig, line, condition)
    breakpoint(...; enter_generated=false)

Add a breakpoint to `f` with the specified argument types `sig`.
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
function breakpoint(f, sig::Type, line::Integer, condition::Condition=nothing; enter_generated=false)
    method = which(f, sig)
    framecode, _ = prepare_framecode(method, sig; enter_generated=enter_generated)
    # Don't use statementnumber(method, line) in case it's enter_generated
    linec = line - whereis(method)[2] + method.line
    stmtidx = statementnumber(framecode, linec)
    breakpoint!(framecode, stmtidx, condition)
end
function breakpoint(f, sig::Type, condition::Condition=nothing; enter_generated=false)
    method = which(f, sig)
    framecode, _ = prepare_framecode(method, sig; enter_generated=enter_generated)
    breakpoint!(framecode, 1, condition)
end

"""
    breakpoint(method::Method)
    breakpoint(method::Method, line)
    breakpoint(method::Method, condition::Expr)
    breakpoint(method::Method, line, condition::Expr)

Add a breakpoint to `method`.
"""
function breakpoint(method::Method, line::Integer, condition::Condition=nothing)
    framecode, stmtidx = statementnumber(method, line)
    breakpoint!(framecode, stmtidx, condition)
end
function breakpoint(method::Method, condition::Condition=nothing)
    framecode = get_framecode(method)
    breakpoint!(framecode, 1, condition)
end

"""
    breakpoint(f)
    breakpoint(f, condition)

Break-on-entry to all methods of `f`.
"""
function breakpoint(f, condition::Condition=nothing)
    bps = BreakpointRef[]
    for method in methods(f)
        push!(bps, breakpoint(method, condition))
    end
    return bps
end

"""
    breakpoint(filename, line)

Set a breakpoint at the specified file and line number.
"""
function breakpoint(filename::AbstractString, line::Integer, args...)
    sigs = signatures_at(filename, line)
    if sigs === nothing
        # TODO: build a Revise-free fallback. Note this won't work well for methods with keywords.
        error("no signatures found at $filename, $line. Restarting and `using Revise` may fix this problem.")
    end
    for sig in sigs
        method = JuliaInterpreter.whichtt(sig)
        method === nothing && continue
        # Check to see if this method really contains that line. Methods that fill in a default positional argument,
        # keyword arguments, and @generated sections may not contain the line.
        _, line1 = whereis(method)
        offset = line1 - method.line
        src = JuliaInterpreter.get_source(method)
        lastline = src.linetable[end]
        if lastline.line + offset >= line
            return breakpoint(method, line, args...)
        end
    end
    error("no signatures found at $filename, $line among the signatures $sigs")
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
