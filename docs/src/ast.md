# Lowered representation

Let's start with a demonstration on simple function:

```julia
function summer(A::AbstractArray{T}) where T
    s = zero(T)
    for a in A
        s += a
    end
    return s
end

A = [1, 2, 5]
```

ASTIntepreter2 uses the lowered representation of code:

```julia
julia> code = @code_lowered summer(A)
CodeInfo(
1 ─       s = (Main.zero)($(Expr(:static_parameter, 1)))
│   %2  = A
│         #temp# = (Base.iterate)(%2)
│   %4  = #temp# === nothing
│   %5  = (Base.not_int)(%4)
└──       goto #4 if not %5
2 ┄ %7  = #temp#
│         a = (Core.getfield)(%7, 1)
│   %9  = (Core.getfield)(%7, 2)
│         s = s + a
│         #temp# = (Base.iterate)(%2, %9)
│   %12 = #temp# === nothing
│   %13 = (Base.not_int)(%12)
└──       goto #4 if not %13
3 ─       goto #2
4 ┄       return s
)
```

To understand this package's internals, you need to familiarize yourself with these
`CodeInfo` objects. The numbers on the left correspond to [basic blocks](https://en.wikipedia.org/wiki/Basic_block);
when used in statements these are printed with a hash, e.g., in `goto #4 if not %6`, the
`#4` refers to basic block 4.
The numbers in the next column--e.g., `%1`, refer to [single static assignment (SSA) values](https://en.wikipedia.org/wiki/Static_single_assignment_form).
Each statement (each line of this printout) corresponds to a single SSA value,
but only those used later in the code are printed using assignment syntax.
Wherever a previous SSA value is used, it's referenced by an `SSAValue` and printed as `%6`;
for example, in `goto #4 if not %6`, the `%6` is the result of evaluating the 6th statement,
which is `(Base.not_int)(%5)`, which in turn refers to the result of statement 5.
Together lines 5 and 6 correspond to `!(#temp# === nothing)`.
(The `#temp#` means that this was a generated variable name not present explicitly in the original source code.)

Before diving into the details, let's first look at the statements themselves:

```julia
julia> code.code
16-element Array{Any,1}:
 :(_3 = (Main.zero)($(Expr(:static_parameter, 1))))
 :(_2)
 :(_4 = (Base.iterate)(%2))
 :(_4 === nothing)
 :((Base.not_int)(%4))
 :(unless %5 goto %16)
 :(_4)
 :(_5 = (Core.getfield)(%7, 1))
 :((Core.getfield)(%7, 2))
 :(_3 = _3 + _5)
 :(_4 = (Base.iterate)(%2, %9))
 :(_4 === nothing)
 :((Base.not_int)(%12))
 :(unless %13 goto %16)
 :(goto %7)
 :(return _3)
```

You can see directly that the SSA assignments are implicit; they are not directly
present in the statement list.
The most noteworthy change here is the appearance of objects like `_3`, which are
references that index into local variable slots:

```julia
julia> code.slotnames
5-element Array{Any,1}:
 Symbol("#self#")
 :A
 :s
 Symbol("#temp#")
 :a
```

When printing the whole `CodeInfo` object, these `slotnames` are substituted in.
The types of objects that can be in `code.code` is well-described in the [Julia AST](https://docs.julialang.org/en/latest/devdocs/ast/) documentation.
