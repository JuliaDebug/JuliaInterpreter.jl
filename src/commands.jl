"""
    pc = finish!(recurse, frame, istoplevel=false)
    pc = finish!(frame, istoplevel=false)

Run `frame` until execution terminates. `pc` is either `nothing` (if execution terminates
when it hits a `return` statement) or a reference to a breakpoint.
In the latter case, `leaf(frame)` returns the frame in which it hit the breakpoint.

`recurse` controls call evaluation; `recurse = Compiled()` evaluates :call expressions
by normal dispatch, whereas the default `recurse = finish_and_return!` uses recursive interpretation.
"""
function finish!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    while true
        pc = step_expr!(recurse, frame, istoplevel)
        (pc === nothing || isa(pc, BreakpointRef)) && return pc
        shouldbreak(frame, pc) && return BreakpointRef(frame.framecode, pc)
    end
end
finish!(frame::Frame, istoplevel::Bool=false) = finish!(finish_and_return!, frame, istoplevel)

"""
    ret = finish_and_return!(recurse, frame, istoplevel::Bool=false)
    ret = finish_and_return!(frame, istoplevel::Bool=false)

Call [`JuliaInterpreter.finish!`](@ref) and pass back the return value `ret`. If execution
pauses at a breakpoint, `ret` is the reference to the breakpoint.
"""
function finish_and_return!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    pc = finish!(recurse, frame, istoplevel)
    isa(pc, BreakpointRef) && return pc
    return get_return(frame)
end
finish_and_return!(frame::Frame, istoplevel::Bool=false) = finish_and_return!(finish_and_return!, frame, istoplevel)

"""
    bpref = dummy_breakpoint(recurse, frame::Frame, istoplevel)

Return a fake breakpoint. `dummy_breakpoint` can be useful as the `recurse` argument to
`evaluate_call!` (or any of the higher-order commands) to ensure that you return immediately
after stepping into a call.
"""
dummy_breakpoint(@nospecialize(recurse), frame::Frame, istoplevel) = BreakpointRef(frame.framecode, 0)

"""
    ret = finish_stack!(recurse, frame, rootistoplevel=false)
    ret = finish_stack!(frame, rootistoplevel=false)

Unwind the callees of `frame`, finishing each before returning to the caller.
`frame` itself is also finished. `rootistoplevel` should be true if the root frame is top-level.

`ret` is typically the returned value. If execution hits a breakpoint, `ret` will be a
reference to the breakpoint.
"""
function finish_stack!(@nospecialize(recurse), frame::Frame, rootistoplevel::Bool=false)
    frame0 = frame
    frame = leaf(frame)
    while true
        istoplevel = rootistoplevel && frame.caller === nothing
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
        shouldbreak(frame, pc) && return BreakpointRef(frame.framecode, pc)
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

"""
    pc = next_call!(recurse, frame, istoplevel=false)
    pc = next_call!(frame, istoplevel=false)

Execute the current statement. Continue stepping through `frame` until the next
`:return` or `:call` expression.
"""
next_call!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false) =
    next_until!(is_call_or_return, recurse, frame, istoplevel)
next_call!(frame::Frame, istoplevel::Bool=false) = next_call!(finish_and_return!, frame, istoplevel)

"""
    pc = maybe_next_call!(recurse, frame, istoplevel=false)
    pc = maybe_next_call!(frame, istoplevel=false)

Return the current program counter of `frame` if it is a `:return` or `:call` expression.
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
    pc = through_methoddef_or_done!(recurse, frame)
    pc = through_methoddef_or_done!(frame)

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

"""
    pc = next_line!(recurse, frame, istoplevel=false)
    pc = next_line!(frame, istoplevel=false)

Execute until reaching the first call of the next line of the source code.
Upon return, `pc` is either the new program counter, `nothing` if a `return` is reached,
or a `BreakpointRef` if it encountered a wrapper call. In the latter case, call `leaf(frame)`
to obtain the new execution frame.
"""
function next_line!(@nospecialize(recurse), frame::Frame, istoplevel::Bool=false)
    pc = frame.pc
    initialline, initialfile = linenumber(frame, pc), getfile(frame, pc)
    first = true
    while linenumber(frame, pc) == initialline && getfile(frame, pc) == initialfile
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
            if switched
                pc = next_line!(recurse, frame, false)
                pc === nothing && error("confusing next_line!")
                lframe = leaf(frame)
                return isa(pc, BreakpointRef) ? pc : BreakpointRef(lframe.framecode, lframe.pc)
            end
        else
            pc = step_expr!(recurse, frame, istoplevel)
            (pc === nothing || isa(pc, BreakpointRef)) && return pc
        end
        shouldbreak(frame, pc) && return BreakpointRef(frame.framecode, pc)
    end
    maybe_next_call!(recurse, frame, istoplevel)
end
next_line!(frame::Frame, istoplevel::Bool=false) = next_line!(finish_and_return!, frame, istoplevel)

"""
    cframe = maybe_step_through_wrapper!(recurse, frame)
    cframe = maybe_step_through_wrapper!(frame)

Return the new frame of execution, potentially stepping through "wrapper" methods like those
that supply default positional arguments or handle keywords. `cframe` is the leaf frame from
which execution should start.
"""
function maybe_step_through_wrapper!(@nospecialize(recurse), frame::Frame)
    code = frame.framecode
    stmts, scope = code.src.code, code.scope::Method
    length(stmts) < 2 && return frame
    last = stmts[end-1]
    isexpr(last, :(=)) && (last = last.args[2])
    is_kw = isa(scope, Method) && startswith(String(Base.unwrap_unionall(Base.unwrap_unionall(scope.sig).parameters[1]).name.name), "#kw")
    if is_kw || isexpr(last, :call) && any(x->x==Core.SlotNumber(1), last.args)
        # If the last expr calls #self# or passes it to an implementation method,
        # this is a wrapper function that we might want to step through
        while frame.pc != length(stmts)-1
            pc = next_call!(recurse, frame, false)  # since we're in a Method we're not at toplevel
            pc === nothing && return frame
        end
        ret = evaluate_call!(dummy_breakpoint, frame, last)
        @assert isa(ret, BreakpointRef)
        return maybe_step_through_wrapper!(recurse, callee(frame))
    end
    return frame
end
maybe_step_through_wrapper!(frame::Frame) = maybe_step_through_wrapper!(finish_and_return!, frame)

"""
    ret = maybe_reset_frame!(recurse, frame, pc, rootistoplevel)

Perform a return to the caller, or descend to the level of a breakpoint.
`pc` is the return state from the previous command (e.g., `next_call!` or similar).
`rootistoplevel` should be true if the root frame is top-level.

`ret` will be `nothing` if we have just completed a top-level frame. Otherwise,

    cframe, cpc = ret

where `cframe` is the frame from which execution should continue and `cpc` is the state
of `cframe` (the program counter, a `BreakpointRef`, or `nothing`).
"""
function maybe_reset_frame!(@nospecialize(recurse), frame::Frame, @nospecialize(pc), rootistoplevel::Bool)
    isa(pc, BreakpointRef) && return leaf(frame), pc
    if pc === nothing
        val = get_return(frame)
        recycle(frame)
        frame = caller(frame)
        frame === nothing && return nothing
        frame.callee = nothing
        maybe_assign!(frame, val)
        frame.pc += 1
        pc = maybe_next_call!(recurse, frame, rootistoplevel && frame.caller===nothing)
        return maybe_reset_frame!(recurse, frame, pc, rootistoplevel)
    end
    return frame, pc
end

# Unwind the stack until an exc is eventually caught, thereby
# returning the frame that caught the exception at the pc of the catch
# or rethrow the error
function unwind_exception(frame::Frame, exc)
    while frame !== nothing
        if !isempty(frame.framedata.exception_frames)
            # Exception caught
            frame.pc = frame.framedata.exception_frames[end]
            frame.framedata.last_exception[] = exc
            return frame
        end
        recycle(frame)
        frame = caller(frame)
        frame === nothing || (frame.callee = nothing)
    end
    rethrow(exc)
end

"""
    ret = debug_command(recurse, frame, cmd, rootistoplevel=false)
    ret = debug_command(frame, cmd, rootistoplevel=false)

Perform one "debugger" command. `cmd` should be one of:

- `:n`: advance to the next line
- `:s`: step into the next call
- `:c`: continue execution until termination or reaching a breakpoint
- `:finish`: finish the current frame and return to the parent

or one of the 'advanced' commands

- `:nc`: step forward to the next call
- `:se`: execute a single statement
- `:si`: execute a single statement, stepping in if it's a call
- `:sg`: step into the generator of a generated function

`rootistoplevel` and `ret` are as described for [`JuliaInterpreter.maybe_reset_frame!`](@ref).
"""
function debug_command(@nospecialize(recurse), frame::Frame, cmd::Symbol, rootistoplevel::Bool=false)
    istoplevel = rootistoplevel && frame.caller === nothing
    cmd0 = cmd
    if cmd == :si
        stmt = pc_expr(frame)
        cmd = is_call(stmt) ? :s : :se
    end
    try
        cmd == :nc && return maybe_reset_frame!(recurse, frame, next_call!(recurse, frame, istoplevel), rootistoplevel)
        cmd == :n && return maybe_reset_frame!(recurse, frame, next_line!(recurse, frame, istoplevel), rootistoplevel)
        cmd == :se && return maybe_reset_frame!(recurse, frame, step_expr!(recurse, frame, istoplevel), rootistoplevel)

        enter_generated = false
        if cmd == :sg
            enter_generated = true
            cmd = :s
        end
        if cmd == :s
            pc = maybe_next_call!(recurse, frame, istoplevel)
            (isa(pc, BreakpointRef) || pc === nothing) && return maybe_reset_frame!(recurse, frame, pc, rootistoplevel)
            stmt0 = stmt = pc_expr(frame, pc)
            isexpr(stmt0, :return) && return maybe_reset_frame!(recurse, frame, nothing, rootistoplevel)
            if isexpr(stmt, :(=))
                stmt = stmt.args[2]
            end
            local ret
            try
                ret = evaluate_call!(dummy_breakpoint, frame, stmt; enter_generated=enter_generated)
            catch err
                ret = handle_err(recurse, frame, err)
                return isa(ret, BreakpointRef) ? (leaf(frame), ret) : ret
            end
            if isa(ret, BreakpointRef)
                newframe = leaf(frame)
                cmd0 == :si && return newframe, ret
                newframe = maybe_step_through_wrapper!(recurse, newframe)
                return newframe, BreakpointRef(newframe.framecode, 0)
            end
            # if we got here, the call returned a value
            maybe_assign!(frame, stmt0, ret)
            frame.pc += 1
            return frame, frame.pc
        end
        if cmd == :c
            r = root(frame)
            ret = finish_stack!(recurse, r, rootistoplevel)
            return isa(ret, BreakpointRef) ? (leaf(r), ret) : nothing
        end
        cmd == :finish && return maybe_reset_frame!(recurse, frame, finish!(recurse, frame, istoplevel), rootistoplevel)
    catch err
        frame = unwind_exception(frame, err)
        if cmd == :c
            return debug_command(recurse, frame, :c, istoplevel)
        else
            return debug_command(recurse, frame, :nc, istoplevel)
        end
    end
    throw(ArgumentError("command $cmd not recognized"))
end
debug_command(frame::Frame, cmd::Symbol, rootistoplevel::Bool=false) =
    debug_command(finish_and_return!, frame, cmd, rootistoplevel)
