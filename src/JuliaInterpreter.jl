module JuliaInterpreter

using Base.Meta
import Base: +, convert, isless
using Core: CodeInfo, SSAValue, SlotNumber, TypeMapEntry, SimpleVector, LineInfoNode, GotoNode, Slot,
            GeneratedFunctionStub, MethodInstance, NewvarNode, TypeName

export @enter, @make_stack, @interpret, Compiled, JuliaStackFrame

"""
`Compiled` is a trait indicating that any `:call` expressions should be evaluated
using Julia's normal compiled-code evaluation. The alternative is to pass `stack=JuliaStackFrame[]`,
which will cause all calls to be evaluated via the interpreter.
"""
struct Compiled end
Base.similar(::Compiled, sz) = Compiled()  # to support similar(stack, 0)

"""
    JuliaProgramCounter(next_stmt::Int)

A wrapper specifying the index of the next statement in the lowered code to be executed.
"""
struct JuliaProgramCounter
    next_stmt::Int
end
+(x::JuliaProgramCounter, y::Integer) = JuliaProgramCounter(x.next_stmt+y)
convert(::Type{Int}, pc::JuliaProgramCounter) = pc.next_stmt
isless(x::JuliaProgramCounter, y::Integer) = isless(x.next_stmt, y)

Base.show(io::IO, pc::JuliaProgramCounter) = print(io, "JuliaProgramCounter(", pc.next_stmt, ')')

# A type used transiently in renumbering CodeInfo SSAValues (to distinguish a new SSAValue from an old one)
struct NewSSAValue
    id::Int
end

"""
`JuliaFrameCode` holds static information about a method or toplevel code.
One `JuliaFrameCode` can be shared by many `JuliaFrameState` calling frames.

Important fields:
- `scope`: the `Method` or `Module` in which this frame is to be evaluated
- `code`: the `CodeInfo` object storing (optimized) lowered code
- `methodtables`: a vector, each entry potentially stores a "local method table" for the corresponding
  `:call` expression in `code` (undefined entries correspond to statements that do not
  contain `:call` expressions)
- `used`: a `BitSet` storing the list of SSAValues that get referenced by later statements.
"""
struct JuliaFrameCode
    scope::Union{Method,Module}
    code::CodeInfo
    methodtables::Vector{TypeMapEntry} # line-by-line method tables for generic-function :call Exprs
    used::BitSet
    wrapper::Bool
    generator::Bool
    # Display options
    fullpath::Bool
end

function JuliaFrameCode(frame::JuliaFrameCode; wrapper = frame.wrapper, generator=frame.generator, fullpath=frame.fullpath)
    JuliaFrameCode(frame.scope, frame.code, frame.methodtables, frame.used,
                   wrapper, generator, fullpath)
end

function JuliaFrameCode(scope, code::CodeInfo; wrapper=false, generator=false, fullpath=true)
    code = optimize!(copy_codeinfo(code), moduleof(scope))
    used = find_used(code)
    methodtables = Vector{TypeMapEntry}(undef, length(code.code))
    return JuliaFrameCode(scope, code, methodtables, used, wrapper, generator, fullpath)
end

"""
`JuliaStackFrame` represents the current execution state in a particular call frame.

Important fields:
- `code`: the [`JuliaFrameCode`](@ref) for this frame
- `locals`: a vector containing the input arguments and named local variables for this frame.
  The indexing corresponds to the names in `frame.code.code.slotnames`.
- `ssavalues`: a vector containing the
  [Static Single Assignment](https://en.wikipedia.org/wiki/Static_single_assignment_form)
  values produced at the current state of execution
- `sparams`: the static type parameters, e.g., for `f(x::Vector{T}) where T` this would store
  the value of `T` given the particular input `x`.
- `pc`: the [`JuliaProgramCounter`](@ref) that typically represents the current position
  during execution. However, note that some internal functions instead maintain the `pc`
  as a local variable, and only update the frame's `pc` when pushing a frame on the stack.
"""
struct JuliaStackFrame
    code::JuliaFrameCode
    locals::Vector{Union{Nothing,Some{Any}}}
    ssavalues::Vector{Any}
    sparams::Vector{Any}
    exception_frames::Vector{Int}
    last_exception::Base.RefValue{Any}
    pc::Base.RefValue{JuliaProgramCounter}
    # A vector from names to the slotnumber of that name
    # for which a reference was last encountered.
    last_reference::Dict{Symbol,Int}
    callargs::Vector{Any}  # a temporary for processing arguments of :call exprs
end

function JuliaStackFrame(framecode::JuliaFrameCode, frame::JuliaStackFrame, pc::JuliaProgramCounter; kwargs...)
    pcref = frame.pc
    pcref[] = pc
    if !isempty(kwargs)
        framecode = JuliaFrameCode(framecode; kwargs...)
    end
    JuliaStackFrame(framecode, frame.locals,
                    frame.ssavalues, frame.sparams,
                    frame.exception_frames, frame.last_exception,
                    pcref, frame.last_reference, frame.callargs)
end

JuliaStackFrame(frame::JuliaStackFrame, pc::JuliaProgramCounter; kwargs...) =
    JuliaStackFrame(frame.code, frame, pc; kwargs...)

"""
`framedict[method]` returns the `JuliaFrameCode` for `method`. For `@generated` methods,
see [`genframedict`](@ref).
"""
const framedict = Dict{Method,JuliaFrameCode}()                # essentially a method table for lowered code

"""
`genframedict[(method,argtypes)]` returns the `JuliaFrameCode` for a `@generated` method `method`,
for the particular argument types `argtypes`.

The framecodes stored in `genframedict` are for the code returned by the generator
(i.e, what will run when you call the method on particular argument types);
for the generator itself, its framecode would be stored in [`framedict`](@ref).
"""
const genframedict = Dict{Tuple{Method,Type},JuliaFrameCode}() # the same for @generated functions

"""
`meth ∈ compiled_methods` indicates that `meth` should be run using [`Compiled`](@ref)
rather than recursed into via the interpreter.
"""
const compiled_methods = Set{Method}()

const junk = JuliaStackFrame[]      # to allow re-use of allocated memory (this is otherwise a bottleneck)

const empty_svec = Core.svec()

include("localmethtable.jl")
include("interpret.jl")
include("builtins.jl")

function show_stackloc(io::IO, stack, frame, pc=frame.pc[])
    indent = ""
    for f in stack
        println(io, indent, f.code.scope)
        indent *= "  "
    end
    println(io, indent, frame.code.scope, ", pc = ", convert(Int, pc))
end
function show_stackloc(io::IO, ::Compiled, frame, pc)
    println(io, "No stack, ::Compiled")
    println(io, frame.code.scope, ", pc = ", convert(Int, pc))
end
show_stackloc(stack, frame, pc=frame.pc[]) = show_stackloc(stderr, stack, frame, pc)

function moduleof(x)
    if isa(x, JuliaStackFrame)
        x = x.code.scope
    end
    return _moduleof(x)
end
_moduleof(scope::Method) = scope.module
_moduleof(scope::Module) = scope
Base.nameof(frame) = isa(frame.code.scope, Method) ? frame.code.scope.name : nameof(frame.code.scope)

is_loc_meta(expr, kind) = isexpr(expr, :meta) && length(expr.args) >= 1 && expr.args[1] === kind

"""
    isglobalref(g, mod, name)

Tests whether `g` is equal to `GlobalRef(mod, name)`.
"""
isglobalref(g, mod, name) = isa(g, GlobalRef) && g.mod === mod && g.name == name

function to_function(x)
    if isa(x, GlobalRef)
        getfield(x.mod, x.name)
    else
        x
    end
end

_Typeof(x) = isa(x,Type) ? Type{x} : typeof(x)

function is_function_def(ex)
    (isexpr(ex, :(=)) && isexpr(ex.args[1], :call)) ||
    isexpr(ex,:function)
end

function is_generated(meth)
    isdefined(meth, :generator)
end

function namedtuple(kwargs)
    names, types, vals = Symbol[], [], []
    for pr in kwargs
        if isa(pr, Expr)
            push!(names, pr.args[1])
            val = pr.args[2]
            push!(types, typeof(val))
            push!(vals, val)
        elseif isa(pr, Pair)
            push!(names, pr.first)
            val = pr.second
            push!(types, typeof(val))
            push!(vals, val)
        else
            error("unhandled entry type ", typeof(pr))
        end
    end
    return NamedTuple{(names...,), Tuple{types...}}(vals)
end

"""
    frun, allargs = prepare_args(fcall, fargs, kwargs)

Prepare the complete argument sequence for a call to `fcall`. `fargs = [fcall, args...]` is a list
containing both `fcall` (the `#self#` slot in lowered code) and the positional
arguments supplied to `fcall`. `kwargs` is a list of keyword arguments, supplied either as
list of expressions `:(kwname=kwval)` or pairs `:kwname=>kwval`.

For non-keyword methods, `frun === fcall`, but for methods with keywords `frun` will be the
keyword-sorter function for `fcall`.

# Example

```jldoctest; setup=(using JuliaInterpreter; empty!(JuliaInterpreter.junk))
julia> mymethod(x) = 1
mymethod (generic function with 1 method)

julia> mymethod(x, y; verbose=false) = nothing
mymethod (generic function with 2 methods)

julia> JuliaInterpreter.prepare_args(mymethod, [mymethod, 15], ())
(mymethod, Any[mymethod, 15])

julia> JuliaInterpreter.prepare_args(mymethod, [mymethod, 1, 2], [:verbose=>true])
(getfield( Symbol("#kw##mymethod"))(), Any[#kw##mymethod(), (verbose = true,), mymethod, 1, 2])
```
"""
function prepare_args(@nospecialize(f), allargs, kwargs)
    if !isempty(kwargs)
        f = Core.kwfunc(f)
        allargs = [f,namedtuple(kwargs),allargs...]
    elseif f === Core._apply
        f = to_function(allargs[2])
        allargs = Base.append_any((allargs[2],), allargs[3:end]...)
    end
    return f, allargs
end

function whichtt(tt)
    m = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), tt, typemax(UInt))
    m === nothing && return nothing
    return m.func::Method
end

"""
    framecode, frameargs, lenv, argtypes = prepare_call(f, allargs; enter_generated=false)

Prepare all the information needed to execute lowered code for `f` given arguments `allargs`.
`f` and `allargs` are the outputs of [`prepare_args`](@ref).
For `@generated` methods, set `enter_generated=true` if you want to extract the lowered code
of the generator itself.

On return `framecode` is the [`JuliaFrameCode`](@ref) of the method.
`frameargs` contains the actual arguments needed for executing this frame (for generators,
this will be the types of `allargs`);
`lenv` is the "environment", i.e., the static parameters for `f` given `allargs`.
`argtypes` is the `Tuple`-type for this specific call (equivalent to the signature of the `MethodInstance`).

# Example

```jldoctest; setup=(using JuliaInterpreter; empty!(JuliaInterpreter.junk))
julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 1 method)

julia> framecode, frameargs, lenv, argtypes = JuliaInterpreter.prepare_call(mymethod, [mymethod, [1.0,2.0]]);

julia> framecode
JuliaInterpreter.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(
1 ─     return 1
), Core.TypeMapEntry[#undef], BitSet([]), false, false, true)

julia> frameargs
2-element Array{Any,1}:
 mymethod
 [1.0, 2.0]

julia> lenv
svec(Float64)

julia> argtypes
Tuple{typeof(mymethod),Array{Float64,1}}
```
"""
function prepare_call(@nospecialize(f), allargs; enter_generated = false)
    # Can happen for thunks created by generated functions
    if isa(f, Core.Builtin) || isa(f, Core.IntrinsicFunction)
        return nothing
    elseif any(x->isa(x, Type) && x <: Vararg, allargs)
        return nothing  # https://github.com/JuliaLang/julia/issues/30995
    end
    argtypes = Tuple{map(_Typeof,allargs)...}
    method = whichtt(argtypes)
    if method === nothing
        # Call it to generate the exact error
        f(allargs[2:end]...)
    end
    args = allargs
    sig = method.sig
    isa(method, TypeMapEntry) && (method = method.func)
    if method.module == Core.Compiler || method ∈ compiled_methods
        return Compiled()
    end
    # Get static parameters
    (ti, lenv::SimpleVector) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                        argtypes, sig)::SimpleVector
    enter_generated &= is_generated(method)
    if is_generated(method) && !enter_generated
        framecode = get(genframedict, (method, argtypes), nothing)
    else
        framecode = get(framedict, method, nothing)
    end
    if framecode === nothing
        if is_generated(method) && !enter_generated
            # If we're stepping into a staged function, we need to use
            # the specialization, rather than stepping through the
            # unspecialized method.
            code = Core.Compiler.get_staged(Core.Compiler.code_for_method(method, argtypes, lenv, typemax(UInt), false))
            generator = false
            if code === nothing
                # The generator threw an error. Let's generate the same error by calling it.
                f(allargs[2:end]...)
            end
        else
            if is_generated(method)
                args = Any[_Typeof(a) for a in args]
                code = get_source(method.generator)
                generator = true
            else
                code = get_source(method)
                generator = false
            end
        end
        framecode = JuliaFrameCode(method, code; generator=generator)
        if is_generated(method) && !enter_generated
            genframedict[(method, argtypes)] = framecode
        else
            framedict[method] = framecode
        end
    end
    return framecode, args, lenv, argtypes
end

"""
    frame = prepare_thunk(mod::Module, expr::Expr)

Prepare `expr` for evaluation in `mod`. `expr` should be a "straightforward" expression,
one that does not require special top-level handling (see [`JuliaInterpreter.prepare_toplevel`](@ref)).
"""
function prepare_thunk(mod::Module, thunk::Expr, recursive=false)
    if isexpr(thunk, :thunk)
        framecode = JuliaFrameCode(mod, thunk.args[1])
    elseif isexpr(thunk, :error)
        error("lowering returned an error, ", thunk)
    elseif recursive
        error("expected thunk expression, got ", thunk.head)
    else
        return prepare_thunk(mod, Meta.lower(mod, thunk), true)
    end
    return prepare_locals(framecode, [])
end

function prepare_thunk((mod, ex)::Tuple{Module,Expr})
    lwr = Meta.lower(mod, ex)
    if isexpr(lwr, :thunk)
        return prepare_thunk(mod, lwr)
    # elseif isexpr(lwr, :toplevel)
    #     return prepare_toplevel!(frames, docexprs, lex, mod, lwr; extract_docexprs=extract_docexprs, filename=filename)
    # elseif isa(lwr, Expr) && (lwr.head == :export || lwr.head == :using || lwr.head == :import)
    #     @show lwr
    #     push!(modexs, (mod, ex, lwr))
    # elseif isa(lwr, Symbol) || isa(lwr, Nothing)
    else
        @show mod ex lwr
        error("lowering did not produce a :thunk Expr")
    end
end

"""
    modexs, docexprs = prepare_toplevel(mod::Module, expr::Expr; extract_docexprs=false)

Break `expr` into a list `modexs` of blocks to be successively executed at top level.
This is used when `expr` defines new structs, new methods, or new modules.

`modexs[i]` is a `(Module, Expr)` tuple. A prototype of how to use these is:

    stack = JuliaStackFrame[]
    for modex in modexs
        frame = JuliaInterpreter.prepare_thunk(modex)
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end

The `while` loop here deserves some explanation. Occasionally, a frame may define new methods
(e.g., anonymous or local functions) and then call those methods. In such cases, running
the entire frame as a single block (e.g., with [`JuliaInterpreter.finish_and_return!`](@ref)
can trigger "method is too new..." errors. Instead, this runs each frame, but returns to the caller
after any new method is defined. When this loop is running at top level (e.g., in the REPL),
this allows the world age to update and thus avoid "method is too new..." errors.

Putting the above nested loop inside a function defeats the entire purpose of
`prepare_toplevel`. If necessary, run that loop as

    Core.eval(somemodule, Expr(:toplevel, quote
        body
    ))

where `body` executes the loop plus any preparatory statements required to make the
necessary variables available at top level in `somemodule`.
"""
function prepare_toplevel(mod::Module, expr::Expr; filename=nothing, kwargs...)
    modexs = Tuple{Module,Expr}[]
    docexprs = Dict{Module,Vector{Expr}}()
    if filename === nothing
        # On Julia 1.2+, the first line of a :toplevel expr may contain line info
        if length(expr.args) >= 1 && isa(expr.args[1], LineNumberNode)
            filename = expr.args[1].file
        else
            filename="toplevel"
        end
    end
    return prepare_toplevel!(modexs, docexprs, mod, expr; filename=filename, kwargs...)
end

"""
    isdocexpr(ex)

Test whether expression `ex` is a `@doc` expression.
"""
function isdocexpr(ex)
    docsym = Symbol("@doc")
    if isexpr(ex, :macrocall)
        a = ex.args[1]
        isglobalref(a, Core, docsym) && return true
        isa(a, Symbol) && a == docsym && return true
        if isexpr(a, :.)
            mod, name = a.args[1], a.args[2]
            return mod === :Core && isa(name, QuoteNode) && name.value == docsym
        end
    end
    return false
end

prepare_toplevel!(modexs, docexprs, mod::Module, ex::Expr; kwargs...) =
    prepare_toplevel!(modexs, docexprs, Expr(:block), mod, ex; kwargs...)

function prepare_toplevel!(modexs, docexprs, lex::Expr, mod::Module, ex::Expr; extract_docexprs=false, filename="toplevel")
    # lex is the expression we'll lower; it will accumulate LineNumberNodes and a
    # single top-level expression. We split blocks, module defs, etc.
    if ex.head == :toplevel || ex.head == :block
        prepare_toplevel!(modexs, docexprs, lex, mod, ex.args; extract_docexprs=extract_docexprs, filename=filename)
    elseif ex.head == :module
        modname = ex.args[2]::Symbol
        newmod = isdefined(mod, modname) ? getfield(mod, modname) : Core.eval(mod, :(module $modname end))
        prepare_toplevel!(modexs, docexprs, lex, newmod, ex.args[3]; extract_docexprs=extract_docexprs, filename=filename)
    elseif extract_docexprs && isdocexpr(ex)
        docexs = get(docexprs, mod, nothing)
        if docexs === nothing
            docexs = docexprs[mod] = Expr[]
        end
        push!(docexs, ex)
        body = ex.args[4]
        if isa(body, Expr) && body.head != :call
            prepare_toplevel!(modexs, docexprs, lex, mod, body; extract_docexprs=extract_docexprs, filename=filename)
        end
    else
        if isempty(lex.args)
            push!(lex.args, isexpr(ex, :macrocall) ? ex.args[2] : LineNumberNode(0, Symbol(filename)))
        end
        push!(lex.args, ex)
        push!(modexs, (mod, copy(lex)))
        empty!(lex.args)
    end
    return modexs, docexprs
end

function prepare_toplevel!(frames, docexprs, lex, mod::Module, args::Vector{Any}; filename="toplevel", kwargs...)
    for a in args
        if isa(a, Expr)
            prepare_toplevel!(frames, docexprs, lex, mod, a; filename=filename, kwargs...)
        elseif isa(a, LineNumberNode)
            if a.file === nothing  # happens with toplevel expressions on Julia 1.2
                push!(lex.args, LineNumberNode(a.line, Symbol(filename)))
            else
                push!(lex.args, a)
            end
        else
            push!(lex.args, a)
        end
    end
end

"""
    framecode, frameargs, lenv, argtypes = determine_method_for_expr(expr; enter_generated = false)

Prepare all the information needed to execute a particular `:call` expression `expr`.
For example, try `JuliaInterpreter.determine_method_for_expr(:($(sum)([1,2])))`.
See [`JuliaInterpreter.prepare_call`](@ref) for information about the outputs.
"""
function determine_method_for_expr(expr; enter_generated = false)
    f = to_function(expr.args[1])
    allargs = expr.args
    # Extract keyword args
    local kwargs = Expr(:parameters)
    if length(allargs) > 1 && isexpr(allargs[2], :parameters)
        kwargs = splice!(allargs, 2)
    end
    f, allargs = prepare_args(f, allargs, kwargs.args)
    return prepare_call(f, allargs; enter_generated=enter_generated)
end

function get_source(meth)
    if isa(meth.source, Array{UInt8,1})
        return ccall(:jl_uncompress_ast, Any, (Any, Any), meth, meth.source)
    else
        return meth.source
    end
end

function get_source(g::GeneratedFunctionStub)
    b = g(g.argnames...)
    b isa CodeInfo && return b
    return eval(b)
end

function copy_codeinfo(code::CodeInfo)
    newcode = ccall(:jl_new_struct_uninit, Any, (Any,), CodeInfo)::CodeInfo
    for (i, name) in enumerate(fieldnames(CodeInfo))
        if isdefined(code, name)
            val = getfield(code, name)
            ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), newcode, i-1, val===nothing || isa(val, Type) ? val : copy(val))
        end
    end
    return newcode
end

const calllike = Set([:call, :foreigncall])

function extract_inner_call!(stmt, idx, once::Bool=false)
    isa(stmt, Expr) || return nothing
    (stmt.head == :toplevel || stmt.head == :thunk) && return nothing
    once |= stmt.head ∈ calllike
    for (i, a) in enumerate(stmt.args)
        isa(a, Expr) || continue
        # Make sure we don't "damage" special syntax that requires literals
        if i == 1 && stmt.head == :foreigncall
            continue
        end
        if i == 2 && stmt.head == :call && stmt.args[1] == :cglobal
            continue
        end
        ret = extract_inner_call!(a, idx, once) # doing this first extracts innermost calls
        ret !== nothing && return ret
        iscalllike = a.head ∈ calllike
        if once && iscalllike
            stmt.args[i] = NewSSAValue(idx)
            return a
        end
    end
    return nothing
end

function replace_ssa!(stmt, ssalookup)
    isa(stmt, Expr) || return nothing
    for (i, a) in enumerate(stmt.args)
        if isa(a, SSAValue)
            stmt.args[i] = SSAValue(ssalookup[a.id])
        elseif isa(a, NewSSAValue)
            stmt.args[i] = SSAValue(a.id)
        else
            replace_ssa!(a, ssalookup)
        end
    end
    return nothing
end

function renumber_ssa!(stmts::Vector{Any}, ssalookup)
    for (i, stmt) in enumerate(stmts)
        if isa(stmt, GotoNode)
            stmts[i] = GotoNode(ssalookup[stmt.label])
        elseif isa(stmt, SSAValue)
            stmts[i] = SSAValue(ssalookup[stmt.id])
        elseif isa(stmt, NewSSAValue)
            stmts[i] = SSAValue(stmt.id)
        elseif isa(stmt, Expr)
            replace_ssa!(stmt, ssalookup)
            if (stmt.head == :gotoifnot || stmt.head == :enter) && isa(stmt.args[end], Int)
                stmt.args[end] = ssalookup[stmt.args[end]]
            end
        end
    end
    return stmts
end

function lookup_global_refs!(ex::Expr)
    (ex.head == :isdefined || ex.head == :thunk || ex.head == :toplevel) && return nothing
    for (i, a) in enumerate(ex.args)
        if isa(a, GlobalRef)
            r = getfield(a.mod, a.name)
            ex.args[i] = QuoteNode(r)
        elseif isa(a, Expr)
            lookup_global_refs!(a)
        end
    end
    return nothing
end

"""
    optimize!(code::CodeInfo, mod::Module)

Perform minor optimizations on the lowered AST in `code` to reduce execution time
of the interpreter.
Currently it looks up `GlobalRef`s (for which it needs `mod` to know the scope in
which this will run) and ensures that no statement includes nested `:call` expressions
(splitting them out into multiple SSA-form statements if needed).
"""
function optimize!(code::CodeInfo, mod::Module)
    code.inferred && error("optimization of inferred code not implemented")
    # TODO: because of builtins.jl, for CodeInfos like
    #   %1 = Core.apply_type
    #   %2 = (%1)(args...)
    # it would be best to *not* resolve the GlobalRef at %1

    ## Replace GlobalRefs with QuoteNodes
    for (i, stmt) in enumerate(code.code)
        if isa(stmt, GlobalRef)
            code.code[i] = QuoteNode(getfield(stmt.mod, stmt.name))
        elseif isa(stmt, Expr)
            if stmt.head == :call && stmt.args[1] == :cglobal  # cglobal requires literals
                continue
            else
                lookup_global_refs!(stmt)
            end
        end
    end

    ## Un-nest :call expressions (so that there will be only one :call per line)
    # This will allow us to re-use args-buffers rather than having to allocate new ones each time.
    old_code, old_codelocs = code.code, code.codelocs
    code.code = new_code = eltype(old_code)[]
    code.codelocs = new_codelocs = Int32[]
    ssainc = fill(1, length(old_code))
    for (i, stmt) in enumerate(old_code)
        loc = old_codelocs[i]
        inner = extract_inner_call!(stmt, length(new_code)+1)
        while inner !== nothing
            push!(new_code, inner)
            push!(new_codelocs, loc)
            ssainc[i] += 1
            inner = extract_inner_call!(stmt, length(new_code)+1)
        end
        push!(new_code, stmt)
        push!(new_codelocs, loc)
    end
    # Fix all the SSAValues and GotoNodes
    ssalookup = cumsum(ssainc)
    renumber_ssa!(new_code, ssalookup)
    code.ssavaluetypes = length(new_code)
    return code
end

function prepare_locals(framecode, argvals::Vector{Any})
    if isa(framecode.scope, Method)
        meth, code = framecode.scope::Method, framecode.code
        ssavt = code.ssavaluetypes
        ng = isa(ssavt, Int) ? ssavt : length(ssavt::Vector{Any})
        nargs = length(argvals)
        if !isempty(junk)
            oldframe = pop!(junk)
            locals, ssavalues, sparams = oldframe.locals, oldframe.ssavalues, oldframe.sparams
            exception_frames, last_reference = oldframe.exception_frames, oldframe.last_reference
            callargs = oldframe.callargs
            last_exception, pc = oldframe.last_exception, oldframe.pc
            resize!(locals, length(code.slotflags))
            resize!(ssavalues, ng)
            # for check_isdefined to work properly, we need sparams to start out unassigned
            resize!(sparams, 0)
            empty!(exception_frames)
            empty!(last_reference)
            last_exception[] = nothing
            pc[] = JuliaProgramCounter(1)
        else
            locals = Vector{Union{Nothing,Some{Any}}}(undef, length(code.slotflags))
            ssavalues = Vector{Any}(undef, ng)
            sparams = Vector{Any}(undef, 0)
            exception_frames = Int[]
            last_reference = Dict{Symbol,Int}()
            callargs = Any[]
            last_exception = Ref{Any}(nothing)
            pc = Ref(JuliaProgramCounter(1))
        end
        for i = 1:meth.nargs
            if meth.isva && i == meth.nargs
                locals[i] = nargs < i ? Some{Any}(()) : (let i=i; Some{Any}(ntuple(k->argvals[i+k-1], nargs-i+1)); end)
                break
            end
            locals[i] = nargs >= i ? Some{Any}(argvals[i]) : Some{Any}(())
        end
        # add local variables initially undefined
        for i = (meth.nargs+1):length(code.slotnames)
            locals[i] = nothing
        end
    else
        code = framecode.code
        locals = Vector{Union{Nothing,Some{Any}}}(undef, length(code.slotflags))
        fill!(locals, nothing)
        ssavalues = Vector{Any}(undef, length(code.code))
        sparams = Any[]
        exception_frames = Int[]
        last_reference = Dict{Symbol,Int}()
        callargs = Any[]
        last_exception = Ref{Any}(nothing)
        pc = Ref(JuliaProgramCounter(1))
    end
    JuliaStackFrame(framecode, locals, ssavalues, sparams, exception_frames, last_exception,
                    pc, last_reference, callargs)
end

"""
    frame = build_frame(framecode::JuliaFrameCode, frameargs, lenv)

Construct a new `JuliaStackFrame` for `framecode`, given lowered-code arguments `frameargs` and
static parameters `lenv`. See [`JuliaInterpreter.prepare_call`](@ref) for information about how to prepare the inputs.
"""
function build_frame(framecode, args, lenv)
    frame = prepare_locals(framecode, args)
    resize!(frame.sparams, length(lenv))
    # Add static parameters to environment
    for i = 1:length(lenv)
        T = lenv[i]
        isa(T, TypeVar) && continue  # only fill concrete types
        frame.sparams[i] = T
    end
    return frame
end

"""
    frame = enter_call_expr(expr; enter_generated=false)

Build a `JuliaStackFrame` ready to execute the expression `expr`. Set `enter_generated=true`
if you want to execute the generator of a `@generated` function, rather than the code that
would be created by the generator.

# Example

```jldoctest; setup=(using JuliaInterpreter; empty!(JuliaInterpreter.junk))
julia> mymethod(x) = x+1
mymethod (generic function with 1 method)

julia> JuliaInterpreter.enter_call_expr(:(\$mymethod(1)))
JuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x) in Main at none:1, CodeInfo(
1 ─ %1 = (\$(QuoteNode(+)))(x, 1)
└──      return %1
), Core.TypeMapEntry[#undef, #undef], BitSet([1]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some(1)], Any[#undef, #undef], Any[], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])

julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 2 methods)

julia> a = [1.0, 2.0]
2-element Array{Float64,1}:
 1.0
 2.0

julia> JuliaInterpreter.enter_call_expr(:(\$mymethod(\$a)))
JuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(
1 ─     return 1
), Core.TypeMapEntry[#undef], BitSet([]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some([1.0, 2.0])], Any[#undef], Any[Float64], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])
```

See [`enter_call`](@ref) for a similar approach not based on expressions.
"""
function enter_call_expr(expr; enter_generated = false)
    r = determine_method_for_expr(expr; enter_generated = enter_generated)
    if isa(r, Tuple)
        return build_frame(r[1:end-1]...)
    end
    nothing
end

"""
    frame = enter_call(f, args...; kwargs...)

Build a `JuliaStackFrame` ready to execute `f` with the specified positional and keyword arguments.

# Example

```jldoctest; setup=(using JuliaInterpreter; empty!(JuliaInterpreter.junk))
julia> mymethod(x) = x+1
mymethod (generic function with 1 method)

julia> JuliaInterpreter.enter_call(mymethod, 1)
JuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x) in Main at none:1, CodeInfo(
1 ─ %1 = ($(QuoteNode(+)))(x, 1)
└──      return %1
), Core.TypeMapEntry[#undef, #undef], BitSet([1]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some(1)], Any[#undef, #undef], Any[], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])

julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 2 methods)

julia> JuliaInterpreter.enter_call(mymethod, [1.0, 2.0])
JuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(
1 ─     return 1
), Core.TypeMapEntry[#undef], BitSet([]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some([1.0, 2.0])], Any[#undef], Any[Float64], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])
```

For a `@generated` function you can use `enter_call((f, true), args...; kwargs...)`
to execute the generator of a `@generated` function, rather than the code that
would be created by the generator.

See [`enter_call_expr`](@ref) for a similar approach based on expressions.
"""
function enter_call(@nospecialize(finfo), @nospecialize(args...); kwargs...)
    if isa(finfo, Tuple)
        f = finfo[1]
        enter_generated = finfo[2]::Bool
    else
        f = finfo
        enter_generated = false
    end
    f, allargs = prepare_args(f, Any[f, args...], kwargs)
    # Can happen for thunks created by generated functions
    if isa(f, Core.Builtin) || isa(f, Core.IntrinsicFunction)
        error(f, " is a builtin or intrinsic")
    end
    r = prepare_call(f, allargs; enter_generated=enter_generated)
    if isa(r, Tuple)
        return build_frame(r[1:end-1]...)
    end
    return nothing
end

function maybe_step_through_wrapper!(stack)
    length(stack[1].code.code.code) < 2 && return stack
    last = stack[1].code.code.code[end-1]
    isexpr(last, :(=)) && (last = last.args[2])
    stack1 = stack[1]
    is_kw = stack1.code.scope isa Method && startswith(String(Base.unwrap_unionall(stack1.code.scope.sig).parameters[1].name.name), "#kw")
    if is_kw || isexpr(last, :call) && any(x->x==SlotNumber(1), last.args)
        # If the last expr calls #self# or passes it to an implementation method,
        # this is a wrapper function that we might want to step through
        frame = stack1
        pc = frame.pc[]
        while pc != JuliaProgramCounter(length(frame.code.code.code)-1)
            pc = next_call!(Compiled(), frame, pc)
        end
        stack[1] = JuliaStackFrame(JuliaFrameCode(frame.code; wrapper=true), frame, pc)
        newcall = Expr(:call, map(x->@lookup(frame, x), last.args)...)
        pushfirst!(stack, enter_call_expr(newcall))
        return maybe_step_through_wrapper!(stack)
    end
    stack
end

lower(mod, arg) = false ? expand(arg) : Meta.lower(mod, arg)

# This is a version of gen_call_with_extracted_types, except that is passes back the call expression
# for further processing.
function extract_args(__module__, ex0)
    if isa(ex0, Expr)
        kws = collect(filter(x->isexpr(x,:kw),ex0.args))
        if !isempty(kws)
            names = []
            values = Tuple(map(x-> begin
                push!(names,x.args[1])
                x.args[2]
            end,kws))
            names = Tuple(names)
            return Expr(:tuple,:(Core.kwfunc($(ex0.args[1]))),
                Expr(:call, NamedTuple{names,typeof(values)}, values),
                map(x->isexpr(x, :parameters) ? QuoteNode(x) : x,
                filter(x->!isexpr(x, :kw),ex0.args))...)
        elseif ex0.head == :.
            return Expr(:tuple, :getproperty, ex0.args...)
        elseif ex0.head == :(<:)
            return Expr(:tuple, :(<:), ex0.args...)
        else
            return Expr(:tuple,
                map(x->isexpr(x,:parameters) ? QuoteNode(x) : x, ex0.args)...)
        end
    end
    if isa(ex0, Expr) && ex0.head == :macrocall # Make @edit @time 1+2 edit the macro by using the types of the *expressions*
        return error("Macros are not supported in @enter")
    end
    ex = lower(__module__, ex0)
    exret = Expr(:none)
    if !isa(ex, Expr)
        return error("expression is not a function call or symbol")
    elseif ex.head == :call
        return Expr(:tuple,
                map(x->isexpr(x,:parameters) ? QuoteNode(x) : x, ex.args)...)
    elseif ex.head == :body
        a1 = ex.args[1]
        if isa(a1, Expr) && a1.head == :call
            a11 = a1.args[1]
            if a11 == :setindex!
                return Expr(:tuple,
                map(x->isexpr(x,:parameters) ? QuoteNode(x) : x, arg.args)...)
            end
        end
    end
    return error("expression is not a function call, "
               * "or is too complex for @enter to analyze; "
               * "break it down to simpler parts if possible")
end

function _make_stack(mod, arg)
    args = try
        extract_args(mod, arg)
    catch e
        return :(throw($e))
    end
    quote
        theargs = $(esc(args))
        stack = [JuliaInterpreter.enter_call_expr(Expr(:call,theargs...))]
        JuliaInterpreter.maybe_step_through_wrapper!(stack)
        stack[1] = JuliaInterpreter.JuliaStackFrame(stack[1], JuliaInterpreter.maybe_next_call!(Compiled(), stack[1]))
        stack
    end
end

macro make_stack(arg)
    _make_stack(__module__, arg)
end

"""
    @interpret f(args; kwargs...)

Evaluate `f` on the specified arguments using the interpreter.

# Example

```jldoctest; setup=(using JuliaInterpreter; empty!(JuliaInterpreter.junk))
julia> a = [1, 7]
2-element Array{Int64,1}:
 1
 7

julia> sum(a)
8

julia> @interpret sum(a)
8
```
"""
macro interpret(arg)
    args = try
        extract_args(__module__, arg)
    catch e
        return :(throw($e))
    end
    quote
        theargs = $(esc(args))
        stack = JuliaStackFrame[]
        frame = JuliaInterpreter.enter_call_expr(Expr(:call, theargs...))
        if frame === nothing
            return eval(Expr(:call, map(QuoteNode, theargs)...))
        end
        empty!(framedict)  # start fresh each time; kind of like bumping the world age at the REPL prompt
        empty!(genframedict)
        finish_and_return!(stack, frame)
    end
end

include("precompile.jl")
_precompile_()

end # module
