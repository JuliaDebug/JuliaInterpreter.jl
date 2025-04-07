# Developer notes

## Handling world age

The authoritative source on toplevel expression evaluation, and how it interacts with world age, is Julia's `src/toplevel.c`.
The following notes are an attempt at distilling some of the rules. In JuliaInterpreter, these apply only
when evaluating :toplevel expressions.

The Task's world age is transiently set to `Base.get_world_counter()` to:

- expand and evaluate each independent expression inside a :module or :toplevel expr
- run module initializers
- perform lowering
- evaluate a `using Foo`/`import Foo` statement
- evaluate toplevel scoping statements `PkgA.f`
- evaluate toplevel thunks
- parsing and evaluating from `include`

The global world counter increments when:

- Evaluating a :global expr
- Declaring a new const (a :const expr, a method definition, a new struct, etc)
- Evaluating an `export` or `public` statement

These all require `eval` or `ccall` statements, since you can't manipulate the world counter directly from Julia.
