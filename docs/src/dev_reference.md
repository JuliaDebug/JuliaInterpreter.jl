# Function reference

## Running the interpreter

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

## Frame traversal

```@docs
root
leaf
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
JuliaInterpreter.evaluate_foreigncall
JuliaInterpreter.maybe_evaluate_builtin
JuliaInterpreter.maybe_next_call!
JuliaInterpreter.next_line!
JuliaInterpreter.next_call!
JuliaInterpreter.maybe_reset_frame!
JuliaInterpreter.maybe_step_through_wrapper!
JuliaInterpreter.maybe_step_through_kwprep!
JuliaInterpreter.handle_err
JuliaInterpreter.debug_command
```

## Breakpoints

```@docs
@breakpoint
@bp
breakpoint
enable
disable
remove
break_on
break_off
JuliaInterpreter.dummy_breakpoint
```

## Types

```@docs
JuliaInterpreter.Frame
JuliaInterpreter.FrameCode
JuliaInterpreter.FrameData
JuliaInterpreter.FrameInstance
JuliaInterpreter.BreakpointState
JuliaInterpreter.BreakpointRef
```

## Internal storage

```@docs
JuliaInterpreter.framedict
JuliaInterpreter.genframedict
JuliaInterpreter.compiled_methods
JuliaInterpreter.compiled_modules
```

## Utilities

```@docs
JuliaInterpreter.@lookup
JuliaInterpreter.is_wrapper_call
JuliaInterpreter.is_doc_expr
JuliaInterpreter.is_global_ref
CodeTracking.whereis
JuliaInterpreter.linenumber
JuliaInterpreter.statementnumber
JuliaInterpreter.Variable
JuliaInterpreter.locals
JuliaInterpreter.whichtt
```
