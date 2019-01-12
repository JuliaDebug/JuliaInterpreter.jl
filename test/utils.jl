using Base.Meta: isexpr
using ASTInterpreter2: JuliaStackFrame
using ASTInterpreter2: pc_expr, lookup_var, _step_expr, eval_rhs, isassign, getlhs,
    collect_args, to_function, enter_call, do_assignment!
using DebuggingUtilities

# Steps through the whole expression using `s`
function step_through(frame)
    state = DebuggerFramework.dummy_state([frame])
    while !isexpr(pc_expr(state.stack[end]), :return)
        execute_command(state, state.stack[1], Val{:s}(), "s")
    end
    return lookup_var(state.stack[end], pc_expr(state.stack[end]).args[1], true)
end

function runframe(frame, pc=frame.pc)
    node = pc_expr(frame, pc)
    while !isexpr(node, :return)
        pc = _step_expr(frame, pc)
        pc === nothing && return nothing
        node = pc_expr(frame, pc)
    end
    return Some{Any}(eval_rhs(frame, node.args[1]))
end

function show_stack(io::IO, stack)
    indent = ""
    for (frame, pc) in stack
        println(indent, frame.scope, " at ", pc)
        indent *= "  "
    end
    return nothing
end
show_stack(stack) = show_stack(stderr, stack)

function runstack(frame::JuliaStackFrame)
    pc = frame.pc
    stack = Tuple{typeof(frame),typeof(pc)}[]
    node = pc_expr(frame, pc)
    local ret
    while true
        if isa(node, Expr)
            if node.head == :return
                ret = eval_rhs(frame, node.args[1])
                if isempty(stack)
                    ret = Some{Any}(ret)
                    break
                end
                frame, pc = stack[end]
                pop!(stack)
                if isassign(frame, pc)
                    do_assignment!(frame, getlhs(pc), ret)
                end
                pc += 1
            elseif node.head == :call
                args = collect_args(frame, node)
                f = to_function(args[1])
                popfirst!(args)
                if f isa Core.Builtin || f isa Core.IntrinsicFunction
                    rhs = f(args...)
                    if isassign(frame, pc)
                        do_assignment!(frame, getlhs(pc), rhs)
                    end
                    pc += 1
                else
                    push!(stack, (frame, pc))
                    frame = enter_call(f, args...)
                    pc = frame.pc
                end
            else
                pc = _step_expr(frame, pc)
            end
        else
            pc = _step_expr(frame, pc)
        end
        node = pc_expr(frame, pc)
    end
    return ret
end
