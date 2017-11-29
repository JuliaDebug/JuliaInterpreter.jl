# Issue #14
stack = @make_stack map(x->2x, 1:10)
state = dummy_state(stack)
execute_command(state, state.stack[1], Val{:finish}(), "finish")
@test isempty(state.stack)
@test state.overall_result == 2 .* [1:10...]
