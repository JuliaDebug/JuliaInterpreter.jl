"""
    ret = finish!(recurse, frame, istoplevel=false)
    ret = finish!(frame, istoplevel=false)

Run `frame` until execution terminates. `ret` is either `nothing` (if execution terminates
when it hits a `return` statement) or a reference to a breakpoint.
In the latter case, `leaf(frame)` returns the frame in which it hit the breakpoint.

`recurse` controls call evaluation; `recurse = Compiled()` evaluates :call expressions
by normal dispatch, whereas the default `recurse = finish_and_return!` uses recursive interpretation.
"""
function finish!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    local pc
    while true
        pc = step_expr!(recurse, frame, istoplevel)
        (pc == nothing || isa(pc, BreakpointRef)) && return pc
        shouldbreak(frame, pc) && return BreakpointRef(frame.framecode, pc)
    end
end
finish!(frame::Frame, istoplevel::Bool=false) = finish!(finish_and_return!, frame, istoplevel)

"""
    ret = finish_and_return!(recurse, frame, istoplevel::Bool=false)
    ret = finish_and_return!(frame, istoplevel::Bool=false)

Call [`JuliaInterpreter.finish!`](@ref) and pass back the return value. If execution
pauses at a breakpoint, the reference to the breakpoint is returned.
"""
function finish_and_return!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    pc = finish!(recurse, frame, istoplevel)
    isa(pc, BreakpointRef) && return pc
    return get_return(frame)
end
finish_and_return!(frame::Frame, istoplevel::Bool=false) = finish_and_return!(finish_and_return!, frame, istoplevel)

"""
    bpref = dummy_breakpoint(recurse, frame::Frame)

Return a fake breakpoint. This can be useful as the `recurse` argument to `evaluate_call!`
(or any of the higher-order commands) to ensure that you return immediately after stepping
into a call.
"""
dummy_breakpoint(@nospecialize(recurse), frame::Frame) = BreakpointRef(frame.framecode, 0)

"""
    ret = finish_stack!(recurse, frame, istoplevel=false)
    ret = finish_stack!(frame, istoplevel=false)

Unwind the callees of `frame`, finishing each before returning to the caller.
`frame` itself is also finished
If execution hits a breakpoint, `ret` will be a reference to the breakpoint.
"""
function finish_stack!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    frame0 = frame
    frame = leaf(frame)
    while true
        ret = finish_and_return!(recurse, frame, istoplevel)
        isa(ret, BreakpointRef) && return ret
        frame === frame0 && return ret
        recycle(frame)
        frame = caller(frame)
        frame === nothing && return ret
        frame.callee = nothing
        pc = frame.pc
        if isassign(frame, pc)
            lhs = getlhs(pc)
            do_assignment!(frame, lhs, ret)
        end
        pc += 1
        frame.pc = pc
        shouldbreak(frame) && return BreakpointRef(frame.framecode, pc)
    end
end
finish_stack!(frame::Frame, istoplevel::Bool=false) = finish_stack!(finish_and_return!, frame, istoplevel)

"""
    pc = next_until!(predicate, recurse, frame, istoplevel=false)
    pc = next_until!(predicate, frame, istoplevel=false)

Execute the current statement. Then step through statements of `frame` until the next
statement satifies `predicate(stmt)`. `pc` will be the index of the statement at which
evaluation terminates, `nothing` (if the frame reached a `return`), or a `BreakpointRef`.
"""
function next_until!(@nospecialize(predicate), @nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    pc = step_expr!(recurse, frame, istoplevel)
    while pc !== nothing && !isa(pc, BreakpointRef)
        if predicate(pc_expr(frame, pc)) || shouldbreak(frame, pc)
            return pc
        end
        pc = step_expr!(recurse, frame, istoplevel)
    end
    return pc
end
next_until!(predicate, frame::Frame, istoplevel::Bool=false) =
    next_until!(predicate, finish_and_return!, frame, istoplevel)

next_call!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false) =
    next_until!(is_call_or_return, recurse, frame, istoplevel)
next_call!(frame::Frame, istoplevel::Bool=false) = next_call!(finish_and_return!, frame, istoplevel)

"""
    maybe_next_call!(predicate, frame, istoplevel=false)

Return the current statement of `frame` if it is a `:return` or `:call` expression.
Otherwise, step through the statements of `frame` until the next `:return` or `:call` expression.
"""
function maybe_next_call!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    pc = frame.pc
    is_call_or_return(pc_expr(frame, pc)) && return pc
    return next_call!(recurse, frame, istoplevel)
end
maybe_next_call!(frame::Frame, istoplevel::Bool=false) =
    maybe_next_call!(finish_and_return!, frame, istoplevel)

"""
    through_methoddef_or_done!(recurse, frame)
    through_methoddef_or_done!(frame)

Runs `frame` at top level until it either finishes (e.g., hits a `return` statement)
or defines a new method.
"""
function through_methoddef_or_done!(@nospecialize(recurse), frame::Frame)
    predicate(stmt) = isexpr(stmt, :method, 3) || isexpr(stmt, :thunk)
    pc = next_until!(predicate, recurse, frame, true)
    (pc === nothing || isa(pc, BreakpointRef)) && return pc
    return step_expr!(recurse, frame, true)  # define the method and return
end
through_methoddef_or_done!(@nospecialize(recurse), t::Tuple{Module,Expr,Frame}) =
    through_methoddef_or_done!(recurse, t[end])
through_methoddef_or_done!(@nospecialize(recurse), modex::Tuple{Module,Expr,Expr}) = Core.eval(modex[1], modex[3])
through_methoddef_or_done!(arg) = through_methoddef_or_done!(finish_and_return!, arg)

function changed_line!(expr, line, fls)
    if length(fls) == 1 && isa(expr, LineNumberNode)
        return expr.line != line
    elseif length(fls) == 1 && isa(expr, Expr) && isexpr(expr, :line)
        return expr.args[1] != line
    else
        if is_loc_meta(expr, :pop_loc)
            pop!(fls)
        elseif is_loc_meta(expr, :push_loc)
            push!(fls,(expr.args[2],0))
        end
        return false
    end
end

function next_line!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    initial = linenumber(frame)
    first = true
    pc = frame.pc
    while linenumber(frame, pc) == initial
        # If this is a return node, interrupt execution
        expr = pc_expr(frame, pc)
        (!first && isexpr(expr, :return)) && return pc
        first = false
        # If this is a goto node, step it and reevaluate
        if is_goto_node(expr)
            pc = step_expr!(recurse, frame, istoplevel)
            (pc === nothing || isa(pc, BreakpointRef)) && return pc
        elseif recurse !== nothing && is_wrapper_call(expr)
            # With splatting it can happen that we do something like ssa = tuple(#self#), _apply(ssa), which
            # confuses the logic here, just step into the first call that's not a builtin
            switched = false
            while is_wrapper_call(expr)
                ret = evaluate_call!(dummy_breakpoint, frame, expr)
                if frame.callee === nothing &&  !isa(ret, BreakpointRef)
                    # This wasn't a real wrapper call
                    if isassign(frame, pc)
                        lhs = getlhs(pc)
                        do_assignment!(frame, lhs, ret)
                    end
                    frame.pc = pc = pc + 1
                    break
                end
                frame = frame.callee
                switched = true
                expr = pc_expr(frame)
            end
            # Signal that we've switched frames
            switched && return BreakpointRef(frame.framecode, frame.pc)
        else
            pc = step_expr!(recurse, frame, istoplevel)
            (pc === nothing || isa(pc, BreakpointRef)) && return pc
        end
        shouldbreak(frame, pc) && return BreakpointRef(frame.framecode, pc)
    end
    maybe_next_call!(recurse, frame, pc)
end
next_line!(frame::Frame, istoplevel::Bool=false) = next_line!(finish_and_return!, frame, istoplevel)
