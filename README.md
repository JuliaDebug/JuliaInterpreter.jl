# JuliaInterpreter

*An interpreter for Julia code.*

| **Documentation**                                                               | **Build Status**                                                                                |
|:-------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------:|
| [![][docs-stable-img]][docs-stable-url] | [![][gh-actions-img]][gh-actions-url]  [![][codecov-img]][codecov-url] |

## Installation

```jl
]add JuliaInterpreter
```

## Usage 
```jl
julia> using JuliaInterpreter

julia> list = [1, 2, 5]
3-element Vector{Int64}:
 1
 2
 5

julia> sum(list)
8

julia> @interpret sum(list)
8
```



[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: https://JuliaDebug.github.io/JuliaInterpreter.jl/stable

[gh-actions-img]: https://github.com/JuliaDebug/JuliaInterpreter.jl/actions/workflows/CI.yml/badge.svg
[gh-actions-url]: https://github.com/JuliaDebug/JuliaInterpreter.jl/actions/workflows/CI.yml

[codecov-img]: https://codecov.io/gh/JuliaDebug/JuliaInterpreter.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaDebug/JuliaInterpreter.jl
