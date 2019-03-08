# Function reference

## Top-level

```@docs
@interpret
```

## Frame creation

```@docs
JuliaInterpreter.enter_call
JuliaInterpreter.enter_call_expr
JuliaInterpreter.prepare_frame
JuliaInterpreter.determine_method_for_expr
JuliaInterpreter.prepare_args
JuliaInterpreter.prepare_call
JuliaInterpreter.prepare_thunk
JuliaInterpreter.split_expressions
JuliaInterpreter.get_call_framecode
JuliaInterpreter.optimize!
```

## Frame execution

```@docs
JuliaInterpreter.Compiled
JuliaInterpreter.step_expr!
JuliaInterpreter.finish!
JuliaInterpreter.finish_and_return!
JuliaInterpreter.finish_stack!
JuliaInterpreter.get_return
JuliaInterpreter.next_until!
JuliaInterpreter.through_methoddef_or_done!
JuliaInterpreter.evaluate_call!
JuliaInterpreter.evaluate_foreigncall!
JuliaInterpreter.maybe_evaluate_builtin
```

## Breakpoints

```@docs
@breakpoint
breakpoint
enable
disable
remove
```

## Types

```@docs
JuliaInterpreter.JuliaStackFrame
JuliaInterpreter.JuliaFrameCode
JuliaInterpreter.JuliaProgramCounter
JuliaInterpreter.BreakpointState
JuliaInterpreter.BreakpointRef
```

## Internal storage

```@docs
JuliaInterpreter.framedict
JuliaInterpreter.genframedict
JuliaInterpreter.compiled_methods
```

## Utilities

```@docs
JuliaInterpreter.@lookup
JuliaInterpreter.iswrappercall
JuliaInterpreter.is_doc_expr
JuliaInterpreter.is_global_ref
JuliaInterpreter.statementnumber
JuliaInterpreter.Variable
JuliaInterpreter.locals
CodeTracking.whereis
```
