using Base.Meta: isexpr

# Steps through the whole expression using `s`
function step_through(frame)
    state = DebuggerFramework.dummy_state([frame])
    while !isexpr(ASTInterpreter2.pc_expr(state.stack[end]), :return)
        execute_command(state, state.stack[1], Val{:s}(), "s")
    end
    return ASTInterpreter2.lookup_var_if_var(state.stack[end], ASTInterpreter2.pc_expr(state.stack[end]).args[1])
end

function runframe(frame, pc=frame.pc)
    node = ASTInterpreter2.pc_expr(frame, pc)
    while !isexpr(node, :return)
        pc = ASTInterpreter2._step_expr(frame, pc)
        pc === nothing && return nothing
        node = ASTInterpreter2.pc_expr(frame, pc)
    end
    return Some(ASTInterpreter2.eval_rhs(frame, node.args[1]))
end
