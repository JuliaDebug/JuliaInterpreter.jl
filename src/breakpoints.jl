module Breakpoints

using ..JuliaInterpreter
using JuliaInterpreter: JuliaFrameCode, JuliaStackFrame,
                        prepare_framecode, framedict, get_source, sparam_syms,
                        linenumber

export breakpoint

# A type that is unique to this package for which there are no valid operations
struct Unassigned end

"""
    Breakpoint(framecode, stmtidx)

A reference to a breakpoint at a particular statement index `stmtidx` in `framecode`.
"""
struct Breakpoint
    framecode::JuliaFrameCode
    stmtidx::Int
end

function Base.show(io::IO, bp::Breakpoint)
    lineno = linenumber(bp.framecode, bp.stmtidx)
    print(io, "breakpoint(", bp.framecode.scope, ", ", lineno, ')')
end

const _breakpoints = Set{Breakpoint}()

function shouldbreak(frame, pc=frame.pc[])
    idx = convert(Int, pc)
    isassigned(frame.code.breakpoints, idx) || return false
    bp = frame.code.breakpoints[idx]
    bp[1] || return false
    slotfunction = bp[2]
    return slotfunction(frame)::Bool
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

truecondition(frame) = true
falsecondition(frame) = false

## The fundamental implementations of breakpoint-setting
function breakpoint!(framecode::JuliaFrameCode, pc, condition::Union{Bool,Expr}=true)
    stmtidx = convert(Int, pc)
    if isa(condition, Bool)
        framecode.breakpoints[stmtidx] = condition ? (true, truecondition) : (false, falsecondition)
    else
        fex = prepare_slotfunction(framecode, condition)
        framecode.breakpoints[stmtidx] = (true, eval(fex))
    end
    bp = Breakpoint(framecode, stmtidx)
    push!(_breakpoints, bp)
    return bp
end
breakpoint!(frame::JuliaStackFrame, pc=frame.pc[], condition::Union{Bool,Expr}=true) =
    breakpoint!(frame.code, pc, condition)

function set_breakpoint_active!(framecode::JuliaFrameCode, active, pc)
    stmtidx = convert(Int, pc)
    _, condition = framecode.breakpoints[stmtidx]
    framecode.breakpoints[stmtidx] = (active, condition)
    active
end
set_breakpoint_active!(frame::JuliaStackFrame, active, pc) =
    set_breakpoint_active!(frame.code, active, pc)

function get_breakpoint_active(framecode::JuliaFrameCode, pc)
    stmtidx = convert(Int, pc)
    return framecode.breakpoints[stmtidx][1]
end
get_breakpoint_active(frame::JuliaStackFrame, pc) =
    get_breakpoint_active(frame.code, pc)

function toggle_breakpoint_active!(framecode::JuliaFrameCode, pc)
    stmtidx = convert(Int, pc)
    active, condition = framecode.breakpoints[stmtidx]
    framecode.breakpoints[stmtidx] = (!active, condition)
    !active
end
toggle_breakpoint_active!(frame::JuliaStackFrame, active, pc) =
    toggle_breakpoint_active!(frame.code, active, pc)

enable(bp::Breakpoint)  = set_breakpoint_active!(bp.framecode, true, bp.stmtidx)
disable(bp::Breakpoint) = set_breakpoint_active!(bp.framecode, false, bp.stmtidx)
function remove(bp::Breakpoint)
    bp.framecode.breakpoints[bp.stmtidx] = (false, falsecondition)
    delete!(_breakpoints, bp)
    return nothing
end

enable() = for bp in _breakpoints enable(bp) end
disable() = for bp in _breakpoints disable(bp) end
function remove()
    for bp in _breakpoints
        bp.framecode.breakpoints[bp.stmtidx] = (false, falsecondition)
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

Add a breakpoint upon entry to `method`. The first will break unconditionally, the second
only if `condition` evaluates to `true`. `condition` should be written in terms of the
arguments and local variables of `method`.
"""
function breakpoint(method::Method, condition::Union{Bool,Expr}=true)
    # FIXME: this duplicates stuff in prepare_call
    framecode = get(framedict, method, nothing)
    if framecode === nothing
        code = get_source(method)
        framecode = JuliaFrameCode(method, code; generator=false)
        framedict[method] = framecode
    end
    breakpoint!(framecode, 1, condition)
end

"""
    breakpoint(f)
    breakpoint(f, condition)

Break-on-entry to all methods of `f`.
"""
function breakpoint(f, condition::Union{Bool,Expr}=true)
    for method in methods(f)
        breakpoint(method, condition)
    end
    nothing
end

end
