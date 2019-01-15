using Base.Meta: isexpr
using ASTInterpreter2: JuliaStackFrame
using ASTInterpreter2: pc_expr, evaluate_call!, finish_and_return!, @eval_rhs
using DebuggingUtilities

# Steps through the whole expression using `s`
function step_through(frame)
    state = DebuggerFramework.dummy_state([frame])
    while !isexpr(pc_expr(state.stack[end]), :return)
        execute_command(state, state.stack[1], Val{:s}(), "s")
    end
    return @eval_rhs(true, state.stack[end], pc_expr(state.stack[end]).args[1])
end

# Execute a frame using Julia's regular compiled-code dispatch for any :call expressions
runframe(frame, pc=frame.pc[]) = Some{Any}(finish_and_return!(Compiled(), frame, pc))

# Execute a frame using the interpreter for all :call expressions (except builtins & intrinsics)
function runstack(frame::JuliaStackFrame, pc=frame.pc[])
    stack = JuliaStackFrame[]
    return Some{Any}(finish_and_return!(stack, frame, pc))
end
