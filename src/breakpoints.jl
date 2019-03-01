module Breakpoints

using ..JuliaInterpreter
using JuliaInterpreter: JuliaFrameCode, JuliaStackFrame, BreakpointState,
                        truecondition, falsecondition, prepare_framecode, get_framecode,
                        sparam_syms, linenumber
using Base.Meta: isexpr
using InteractiveUtils

export @breakpoint, breakpoint, enable, disable, remove

# A type that is unique to this package for which there are no valid operations
struct Unassigned end

"""
    BreakpointRef(framecode, stmtidx)

A reference to a breakpoint at a particular statement index `stmtidx` in `framecode`.
"""
struct BreakpointRef
    framecode::JuliaFrameCode
    stmtidx::Int
end

function Base.show(io::IO, bp::BreakpointRef)
    lineno = linenumber(bp.framecode, bp.stmtidx)
    print(io, "breakpoint(", bp.framecode.scope, ", ", lineno, ')')
end

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
    return bp.condition(frame)::Bool
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


## The fundamental implementations of breakpoint-setting
function breakpoint!(framecode::JuliaFrameCode, pc, condition::Union{Bool,Expr}=true)
    stmtidx = convert(Int, pc)
    if isa(condition, Bool)
        framecode.breakpoints[stmtidx] = BreakpointState(condition)
    else
        fex = prepare_slotfunction(framecode, condition)
        framecode.breakpoints[stmtidx] = BreakpointState(true, eval(fex))
    end
    return add_breakpoint(framecode, stmtidx)
end
breakpoint!(frame::JuliaStackFrame, pc=frame.pc[], condition::Union{Bool,Expr}=true) =
    breakpoint!(frame.code, pc, condition)

enable(bp::BreakpointRef)  = bp[] = true
disable(bp::BreakpointRef) = bp[] = false
function remove(bp::BreakpointRef)
    idx = findfirst(isequal(bp), _breakpoints)
    deleteat!(_breakpoints, idx)
    bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(false, falsecondition)
    return nothing
end

enable() = for bp in _breakpoints enable(bp) end
disable() = for bp in _breakpoints disable(bp) end
function remove()
    for bp in _breakpoints
        bp.framecode.breakpoints[bp.stmtidx] = BreakpointState(false, falsecondition)
    end
    empty!(_breakpoints)
    return nothing
end

"""
    breakpoint(f, sig)
    breakpoint(f, sig, condition)
    breakpoint(...; enter_generated=false)

Add a breakpoint upon entry to `f` with the specified argument types `sig`.
The first will break unconditionally, the second only if `condition` evaluates to `true`.
`condition` should be written in terms of the arguments and local variables of `f`.

# Example
```julia
function radius2(x, y)
    return x^2 + y^2
end

breakpoint(radius2, Tuple{Int,Int}, :(y > x))
```
"""
function breakpoint(f, sig::Type, condition::Union{Bool,Expr}=true; enter_generated=false)
    method = which(f, sig)
    framecode, _ = prepare_framecode(method, sig; enter_generated=enter_generated)
    breakpoint!(framecode, 1, condition)
end

"""
    breakpoint(method::Method)
    breakpoint(method::Method, condition::Expr)

Add a breakpoint upon entry to `method`.
"""
function breakpoint(method::Method, condition::Union{Bool,Expr}=true)
    framecode = get_framecode(method)
    breakpoint!(framecode, 1, condition)
end

"""
    breakpoint(f)
    breakpoint(f, condition)

Break-on-entry to all methods of `f`.
"""
function breakpoint(f, condition::Union{Bool,Expr}=true)
    bps = BreakpointRef[]
    for method in methods(f)
        push!(bps, breakpoint(method, condition))
    end
    return bps
end

end
