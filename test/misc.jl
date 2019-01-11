# Issue #14
stack = @make_stack map(x->2x, 1:10)
state = dummy_state(stack)
execute_command(state, state.stack[1], Val{:finish}(), "finish")
@test isempty(state.stack)
@test state.overall_result == 2 .* [1:10...]

# Issue #12
function complicated_keyword_stuff(args...; kw...)
    args[1] == args[1]
    (args..., kw...)
end
stack = @make_stack complicated_keyword_stuff(1)
state = dummy_state(stack)
execute_command(state, state.stack[1], Val{:n}(), "n")
execute_command(state, state.stack[1], Val{:finish}(), "finish")
@test isempty(state.stack)

meth = first(methods(complicated_keyword_stuff))
@test ASTInterpreter2.JuliaStackFrame(meth) isa ASTInterpreter2.JuliaStackFrame
