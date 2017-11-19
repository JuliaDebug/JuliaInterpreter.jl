using DebuggerFramework
using ASTInterpreter2

# Simple evaling of function argument
function evalfoo1(x,y)
    x+y
end
frame = ASTInterpreter2.enter_call_expr(:($(evalfoo1)(1,2)))
res = DebuggerFramework.eval_code(nothing, frame, "x")
@assert res == 1

res = DebuggerFramework.eval_code(nothing, frame, "y")
@assert res == 2

# Evaling with sparams
function evalsparams(x::T) where T
    x
end
frame = ASTInterpreter2.enter_call_expr(:($(evalsparams)(1)))
res = DebuggerFramework.eval_code(nothing, frame, "x")
@assert res == 1

res = DebuggerFramework.eval_code(nothing, frame, "T")
@assert res == Int
