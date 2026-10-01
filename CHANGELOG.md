# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- links start -->
[Unreleased]: https://github.com/JuliaDebug/JuliaInterpreter.jl/compare/v0.11.5...HEAD
[0.11.5]: https://github.com/JuliaDebug/JuliaInterpreter.jl/compare/v0.11.4...v0.11.5
[0.11.4]: https://github.com/JuliaDebug/JuliaInterpreter.jl/compare/v0.11.3...v0.11.4
[0.11.3]: https://github.com/JuliaDebug/JuliaInterpreter.jl/compare/v0.11.2...v0.11.3
[0.11.2]: https://github.com/JuliaDebug/JuliaInterpreter.jl/compare/v0.11.1...v0.11.2
[0.11.1]: https://github.com/JuliaDebug/JuliaInterpreter.jl/compare/v0.11.0...v0.11.1
[0.11.0]: https://github.com/JuliaDebug/JuliaInterpreter.jl/compare/v0.10.12...v0.11.0
<!-- links end -->

## [Unreleased]

### Changed
- Added this changelog (JuliaDebug/JuliaInterpreter.jl#780).

### Fixed
- `@cfunction` and top-level `ccall`s run through compiled wrappers instead of
  being compiled on every execution, and top-level `@cfunction` no longer
  errors (JuliaDebug/JuliaInterpreter.jl#773).
- A `throw(nothing)` inside a `catch` block is no longer mistaken for a
  `rethrow()` (JuliaDebug/JuliaInterpreter.jl#774).
- Updated the builtins for Julia 1.14 (JuliaDebug/JuliaInterpreter.jl#778).

## [0.11.5]

### Changed
- Improved interpretation performance by reusing cached frames for interpreted
  calls, tracking frame recycling more cheaply, and optimizing global reference
  lookups again (JuliaDebug/JuliaInterpreter.jl#759,
  JuliaDebug/JuliaInterpreter.jl#761, JuliaDebug/JuliaInterpreter.jl#770).
- Stepping avoids stopping at internal-looking positions
  (JuliaDebug/JuliaInterpreter.jl#760).

### Fixed
- Fixed compiled `:foreigncall` wrappers for parametric argument types and
  nested `UnionAll` types (JuliaDebug/JuliaInterpreter.jl#767,
  JuliaDebug/JuliaInterpreter.jl#769).
- Every handler that catches a rethrown exception now clears the rethrow marker
  (JuliaDebug/JuliaInterpreter.jl#772).

## [0.11.4]

### Added
- Interpreted code can create and call opaque closures.

### Fixed
- Fixed a large number of divergences from native execution, found by an audit
  and a triage of the issue backlog (JuliaDebug/JuliaInterpreter.jl#751,
  JuliaDebug/JuliaInterpreter.jl#752, JuliaDebug/JuliaInterpreter.jl#753,
  JuliaDebug/JuliaInterpreter.jl#755). Notably:
  - The active-exception stack (`rethrow()`, `current_exceptions()`,
    `:pop_exception`) and dynamic scopes now match native execution when
    exceptions unwind, including through the debugger.
  - Builtins that take callbacks, `invoke`, and `applicable` run in the
    frame's world.
  - Fixes to breakpoints, `eval_code`, `ExprSplitter`, and stepping: e.g.
    `next_line!` stops on assignment-only lines, and `:sg` enters generators
    with argument types.
  - Nested `@interpret` works, since the interpreter's own methods run compiled.
- Re-interpreting a module with itself as the parent context re-enters it
  instead of creating a nested module (JuliaDebug/JuliaInterpreter.jl#758).

## [0.11.3]

### Added
- Added support for the `Core.define_method` builtin of Julia 1.14
  (JuliaDebug/JuliaInterpreter.jl#748).

### Changed
- A nested `module X` resolves to a fresh local module, unless it is evaluated
  into `Base.__toplevel__` (a package loading itself) or `X` is already a
  submodule of the parent (JuliaDebug/JuliaInterpreter.jl#740,
  timholy/Revise.jl#747).

### Fixed
- Recycled frames are pooled at most once (JuliaDebug/JuliaInterpreter.jl#749).

## [0.11.2]

### Added
- Added support for Julia 1.14 lowering and runtime changes: `Expr(:foreignglobal)`,
  envout markers, and new builtins (JuliaDebug/JuliaInterpreter.jl#742,
  JuliaDebug/JuliaInterpreter.jl#747).

### Fixed
- Extensions of different packages sharing a name now resolve to the right
  module (JuliaDebug/JuliaInterpreter.jl#743).

## [0.11.1]

### Fixed
- `Core._apply_iterate` runs in the frame's world
  (JuliaDebug/JuliaInterpreter.jl#741).

## [0.11.0]

### Added
- Added support for Julia 1.13 (JuliaDebug/JuliaInterpreter.jl#737).

### Changed
- **Breaking**: Interpretation respects world age. A frame captures a world
  when it is constructed and dispatches and reads globals in it. By default
  this is the caller's task world, as for compiled code, rather than the latest
  world. Top-level frames still advance to the latest world for each statement.
  `@interpret` accepts `world=:latest` or a world number to reach newer
  definitions (JuliaDebug/JuliaInterpreter.jl#715,
  JuliaDebug/JuliaInterpreter.jl#732, JuliaDebug/JuliaInterpreter.jl#738).
- `Frame(mod, ex)` interprets a whole `:toplevel` or `:module` expression
  directly as a single frame tree, instead of relying on `ExprSplitter` to
  return to the actual top level. `ExprSplitter` remains available
  (JuliaDebug/JuliaInterpreter.jl#719).
- The interpreter's caches are invalidated by world age, and are no longer
  cleared on every entry (JuliaDebug/JuliaInterpreter.jl#721,
  JuliaDebug/JuliaInterpreter.jl#733).
- `invokelatest` calls expanded by the recursive interpreter run in the latest
  world (JuliaDebug/JuliaInterpreter.jl#729).
