# Function reference

## Top-level

```@docs
@interpret
```

## Frame creation

```@docs
ASTInterpreter2.enter_call
ASTInterpreter2.enter_call_expr
ASTInterpreter2.build_frame
ASTInterpreter2.determine_method_for_expr
ASTInterpreter2.prepare_args
ASTInterpreter2.prepare_call
ASTInterpreter2.get_call_framecode
ASTInterpreter2.optimize!
```

## Frame execution

```@docs
ASTInterpreter2.Compiled
ASTInterpreter2.step_expr!
ASTInterpreter2.finish!
ASTInterpreter2.finish_and_return!
ASTInterpreter2.next_until!
ASTInterpreter2.evaluate_call!
ASTInterpreter2.evaluate_foreigncall!
ASTInterpreter2.maybe_evaluate_builtin
ASTInterpreter2.@eval_rhs
```

## Types

```@docs
ASTInterpreter2.JuliaStackFrame
ASTInterpreter2.JuliaFrameCode
ASTInterpreter2.JuliaProgramCounter
```

## Internal storage

```@docs
ASTInterpreter2.framedict
ASTInterpreter2.genframedict
```

## Utilities

```@docs
ASTInterpreter2.iswrappercall
```
