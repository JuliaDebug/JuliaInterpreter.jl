# Function reference

## Top-level

```@docs
@interpret
```

## Frame creation

```@docs
JuliaInterpreter.enter_call
JuliaInterpreter.enter_call_expr
JuliaInterpreter.build_frame
JuliaInterpreter.determine_method_for_expr
JuliaInterpreter.prepare_args
JuliaInterpreter.prepare_call
JuliaInterpreter.get_call_framecode
JuliaInterpreter.optimize!
```

## Frame execution

```@docs
JuliaInterpreter.Compiled
JuliaInterpreter.step_expr!
JuliaInterpreter.finish!
JuliaInterpreter.finish_and_return!
JuliaInterpreter.next_until!
JuliaInterpreter.evaluate_call!
JuliaInterpreter.evaluate_foreigncall!
JuliaInterpreter.maybe_evaluate_builtin
```

## Types

```@docs
JuliaInterpreter.JuliaStackFrame
JuliaInterpreter.JuliaFrameCode
JuliaInterpreter.JuliaProgramCounter
```

## Internal storage

```@docs
JuliaInterpreter.framedict
JuliaInterpreter.genframedict
```

## Utilities

```@docs
JuliaInterpreter.iswrappercall
```
