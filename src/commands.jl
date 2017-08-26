function DebuggerFramework.execute_command(state, frame::JuliaStackFrame, ::Union{Val{:ns},Val{:nc},Val{:n},Val{:se}}, command)
    if (pc = command == "ns" ? next_statement!(frame) :
           command == "nc" ? next_call!(frame) :
           command == "n" ? next_line!(frame; state = state) :
            !step_expr(frame)) != nothing #= command == "se" =#
        state.stack[end] = JuliaStackFrame(frame, pc)
        return true
    end
    pop!(state.stack)
    return true
end