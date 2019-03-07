"""
    pc = finish!(stack, frame, pc=frame.pc[])

Run `frame` until execution terminates. `pc` is the program counter for the final statement.
`stack` controls call evaluation; `stack = Compiled()` evaluates :call expressions
by normal dispatch, whereas a vector of `JuliaStackFrame`s will use recursive interpretation.

If execution hits a breakpoint, then `pc` is a reference to the breakpoint. `stack[end]`,
if not running in `Compiled()` mode, will contain the frame in which the breakpoint was hit.
"""
function finish!(stack, frame, pc::JuliaProgramCounter=frame.pc[], istoplevel::Bool=false)
    local new_pc
    while true
        new_pc = _step_expr!(stack, frame, pc, istoplevel)
        (new_pc == nothing || isa(new_pc, BreakpointRef)) && break
        pc = new_pc
        if shouldbreak(frame, pc)
            push!(stack, frame)
            new_pc = BreakpointRef(frame.code, pc)
            break
        end
    end
    frame.pc[] = pc
    return isa(new_pc, BreakpointRef) ? new_pc : pc
end
finish!(stack, frame, istoplevel::Bool) = finish!(stack, frame, frame.pc[], istoplevel)

"""
    ret = finish_and_return!(stack, frame, istoplevel::Bool=false)
    ret = finish_and_return!(stack, frame, pc, istoplevel::Bool)

Run `frame` until execution terminates, and pass back the computed return value.
`stack` controls call evaluation; `stack = Compiled()` evaluates :call expressions
by normal dispatch, whereas a vector of `JuliaStackFrame`s will use recursive interpretation.

If execution hits a breakpoint, then `ret` is a reference to the breakpoint. `stack[end]`,
if not running in `Compiled()` mode, will contain the frame in which the breakpoint was hit.

Optionally supply the starting `pc`, if you don't want to start at the current location in `frame`.
"""
function finish_and_return!(stack, frame, pc::JuliaProgramCounter=frame.pc[], istoplevel::Bool=false)
    pc = finish!(stack, frame, pc, istoplevel)
    isa(pc, BreakpointRef) && return pc
    return get_return(frame, pc)
end
finish_and_return!(stack, frame, istoplevel::Bool) = finish_and_return!(stack, frame, frame.pc[], istoplevel)

"""
    ret = finish_stack!(stack)

Completely unwind `stack`, finishing it frame-by-frame. If execution hits a breakpoint,
`ret` will be a reference to the breakpoint.
"""
function finish_stack!(stack)
    while !isempty(stack)
        frame = pop!(stack)
        ret = finish_and_return!(stack, frame)
        isa(ret, BreakpointRef) && return ret
        isempty(stack) && return ret
        parentframe = stack[end]
        pc = parentframe.pc[]
        if isassign(parentframe, pc)
            lhs = getlhs(pc)
            do_assignment!(parentframe, lhs, ret)
        end
        pc += 1
        parentframe.pc[] = pc
        shouldbreak(parentframe) && return BreakpointRef(parentframe.code, pc)
    end
    error("stack is empty")
end

"""
    next_until!(predicate, stack, frame, pc=frame.pc[])

Step through statements of `frame` until the next statement satifies `predicate(stmt)`.
"""
function next_until!(f, stack, frame, pc::JuliaProgramCounter=frame.pc[], istoplevel::Bool=false)
    while (newpc = _step_expr!(stack, frame, pc, istoplevel)) != nothing
        pc = newpc
        if f(pc_expr(frame, pc)) || shouldbreak(frame, pc)
            frame.pc[] = pc
            return pc
        end
    end
    frame.pc[] = pc
    return nothing
end
next_until!(f, stack, frame, istoplevel::Bool) = next_until!(f, stack, frame, frame.pc[], istoplevel)
next_call!(stack, frame, pc=frame.pc[]) = next_until!(node->is_call(node)||isexpr(node,:return), stack, frame, pc)

"""
    through_methoddef_or_done!(stack, frame)

Runs `frame` at top level until it either finishes (e.g., hits a `return` statement)
or defines a new method.
"""
function through_methoddef_or_done!(stack, frame::JuliaStackFrame)
    predicate(stmt) = isexpr(stmt, :method, 3) || isexpr(stmt, :thunk)
    pc = next_until!(predicate, stack, frame, true)
    pc === nothing && return nothing
    stmt = pc_expr(frame, pc)
    return isexpr(stmt, :method, 3) ? #= normal exit =# _step_expr!(stack, frame, pc, true) :
                                      #= breakpoint exit =# pc
end
through_methoddef_or_done!(stack, frame::Tuple{Module,Expr,JuliaStackFrame}) =
    through_methoddef_or_done!(stack, frame[end])
through_methoddef_or_done!(stack, modex::Tuple{Module,Expr,Expr}) = Core.eval(modex[1], modex[3])

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

function next_line!(stack, frame, dbstack = nothing)
    initial = linenumber(frame)
    first = true
    pc = frame.pc[]
    while linenumber(frame, pc) == initial
        # If this is a return node, interrupt execution. This is the same
        # special case as in `s`.
        expr = pc_expr(frame, pc)
        (!first && isexpr(expr, :return)) && return pc
        first = false
        # If this is a goto node, step it and reevaluate
        if isgotonode(expr)
            pc = _step_expr!(stack, frame, pc)
            (pc == nothing || isa(pc, BreakpointRef)) && return pc
        elseif dbstack !== nothing && iswrappercall(expr)
            # With splatting it can happen that we do something like ssa = tuple(#self#), _apply(ssa), which
            # confuses the logic here, just step into the first call that's not a builtin
            while true
                dbstack[end] = JuliaStackFrame(JuliaFrameCode(frame.code; wrapper = true), frame, pc)
                call_expr = pc_expr(frame, pc)
                isexpr(call_expr, :(=)) && (call_expr = call_expr.args[2])
                call_expr = Expr(:call, map(x->@lookup(frame, x), call_expr.args)...)
                new_frame = enter_call_expr(call_expr)
                if new_frame !== nothing
                    push!(dbstack, new_frame)
                    frame = new_frame
                    pc = frame.pc[]
                    break
                else
                    pc = _step_expr!(stack, frame, pc)
                    (pc == nothing || isa(pc, BreakpointRef)) && return pc
                end
            end
        else
            pc = _step_expr!(stack, frame, pc)
            (pc == nothing || isa(pc, BreakpointRef)) && return pc
        end
        frame.pc[] = pc
        shouldbreak(frame, pc) && return BreakpointRef(frame.code, pc)
    end
    maybe_next_call!(stack, frame, pc)
end

function maybe_next_call!(stack, frame, pc)
    call_or_return(node) = is_call(node) || isexpr(node, :return)
    call_or_return(pc_expr(frame, pc)) ||
        (pc = next_until!(call_or_return, stack, frame, pc, false))
    pc
end
maybe_next_call!(stack, frame) = maybe_next_call!(stack, frame, frame.pc[])
