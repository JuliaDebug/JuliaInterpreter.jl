# ASTInterpreter2

This package implements an [interpreter](https://en.wikipedia.org/wiki/Interpreter_(computing)) for Julia code.
Normally, Julia compiles your code when you first execute it; using ASTInterpreter2 you can
avoid compilation and execute the expressions that define your code directly.
Interpreters have a number of applications, including support for stepping debuggers.

At a pure user level, there is not much to know:

```jldoctest
julia> using ASTInterpreter2

julia> a = [1, 2, 5]
3-element Array{Int64,1}:
 1
 2
 5

julia> sum(a)
8

julia> @interpret sum(a)
8
```

Those who want to dive deeper should continue reading.
