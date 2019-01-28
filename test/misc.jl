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

@test runframe(ASTInterpreter2.enter_call(complicated_keyword_stuff, 1, 2)) ==
      runframe(@make_stack(complicated_keyword_stuff(1, 2))[1])
@test runframe(ASTInterpreter2.enter_call(complicated_keyword_stuff, 1, 2; x=7, y=33)) ==
      runframe(@make_stack(complicated_keyword_stuff(1, 2; x=7, y=33))[1])

# Issue #22
f22() = string(:(a+b))
@test step_through(enter_call_expr(:($f22()))) == "a + b"
f22() = string(QuoteNode(:a))
@test step_through(enter_call_expr(:($f22()))) == ":a"
