"""
`framedict[method]` returns the `FrameCode` for `method`. For `@generated` methods,
see [`genframedict`](@ref).
"""
const framedict = Dict{Method,FrameCode}()                # essentially a method table for lowered code

"""
`genframedict[(method,argtypes)]` returns the `FrameCode` for a `@generated` method `method`,
for the particular argument types `argtypes`.

The framecodes stored in `genframedict` are for the code returned by the generator
(i.e, what will run when you call the method on particular argument types);
for the generator itself, its framecode would be stored in [`framedict`](@ref).
"""
const genframedict = Dict{Tuple{Method,Type},FrameCode}() # the same for @generated functions

"""
`meth ∈ compiled_methods` indicates that `meth` should be run using [`Compiled`](@ref)
rather than recursed into via the interpreter.
"""
const compiled_methods = Set{Method}()

"""
`mod ∈ compiled_modules` indicates that any method in `mod` should be run using [`Compiled`](@ref)
rather than recursed into via the interpreter.
"""
const compiled_modules = Set{Module}()

const junk = FrameData[] # to allow re-use of allocated memory (this is otherwise a bottleneck)
const debug_recycle = Base.RefValue(false)
@noinline _check_frame_not_in_junk(frame) = @assert frame.framedata ∉ junk
@inline function recycle(frame)
    debug_recycle[] && _check_frame_not_in_junk(frame)
    push!(junk, frame.framedata)
end

const empty_svec = Core.svec()

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

get_source(meth::Method) = Base.uncompressed_ast(meth)

function get_source(g::GeneratedFunctionStub, env)
    b = g(env..., g.argnames...)
    b isa CodeInfo && return b
    return eval(b)
end

function copy_codeinfo(code::CodeInfo)
    @static if VERSION < v"1.1.0-DEV.762"
        newcode = ccall(:jl_new_struct_uninit, Any, (Any,), CodeInfo)::CodeInfo
        for (i, name) in enumerate(fieldnames(CodeInfo))
            if isdefined(code, name)
                val = getfield(code, name)
                ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), newcode, i-1, val===nothing || isa(val, Union{Type, Method}) ? val : copy(val))
            end
        end
        return newcode
    else
        # Inline this when support for VERSION above is dropped
        return copy(code)
    end
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
        allargs = [f, namedtuple(kwargs), allargs...]
    elseif f === Core._apply
        f = to_function(allargs[2])
        allargs = Base.append_any((allargs[2],), allargs[3:end]...)
    end
    return f, allargs
end

if VERSION < v"1.2-" || !isdefined(Core.Compiler, :specialize_method)
    specialize_method(method::Method, @nospecialize(atypes), sparams::SimpleVector) =
        Core.Compiler.code_for_method(method, atypes, sparams, typemax(UInt))
else
    const specialize_method = Core.Compiler.specialize_method
end

function prepare_framecode(method::Method, @nospecialize(argtypes); enter_generated=false)
    sig = method.sig
    if method.module ∈ compiled_modules || method ∈ compiled_methods
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
            code = Core.Compiler.get_staged(specialize_method(method, argtypes, lenv))
            code === nothing && return nothing
            generator = false
        else
            if is_generated(method)
                code = get_source(method.generator, lenv)
                generator = true
            else
                code = get_source(method)
                generator = false
            end
        end
        framecode = FrameCode(method, code; generator=generator)
        if is_generated(method) && !enter_generated
            genframedict[(method, argtypes)] = framecode
        else
            framedict[method] = framecode
        end
    end
    return framecode, lenv
end

function get_framecode(method)
    framecode = get(framedict, method, nothing)
    if framecode === nothing
        code = get_source(method)
        framecode = FrameCode(method, code; generator=false)
        framedict[method] = framecode
    end
    return framecode
end

"""
    framecode, frameargs, lenv, argtypes = prepare_call(f, allargs; enter_generated=false)

Prepare all the information needed to execute lowered code for `f` given arguments `allargs`.
`f` and `allargs` are the outputs of [`prepare_args`](@ref).
For `@generated` methods, set `enter_generated=true` if you want to extract the lowered code
of the generator itself.

On return `framecode` is the [`FrameCode`](@ref) of the method.
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
  1  1  1 ─     return 1

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
    elseif any(is_vararg_type, allargs)
        return nothing  # https://github.com/JuliaLang/julia/issues/30995
    end
    argtypesv = Any[_Typeof(a) for a in allargs]
    argtypes = Tuple{argtypesv...}
    method = whichtt(argtypes)
    if method === nothing
        # Call it to generate the exact error
        f(allargs[2:end]...)
    end
    args = allargs
    ret = prepare_framecode(method, argtypes; enter_generated=enter_generated)
    # Exceptional returns
    if ret === nothing
        # The generator threw an error. Let's generate the same error by calling it.
        f(allargs[2:end]...)
    end
    isa(ret, Compiled) && return ret
    # Typical return
    framecode, lenv = ret
    if is_generated(method) && enter_generated
        args = Any[_Typeof(a) for a in args]
    end
    return framecode, args, lenv, argtypes
end

function prepare_framedata(framecode, argvals::Vector{Any}, caller_will_catch_err::Bool=false)
    if isa(framecode.scope, Method)
        meth, src = framecode.scope::Method, framecode.src
        slotnames = src.slotnames::SlotNamesType
        ssavt = src.ssavaluetypes
        ng = isa(ssavt, Int) ? ssavt : length(ssavt::Vector{Any})
        nargs, meth_nargs = length(argvals), Int(meth.nargs)
        if length(junk) > 0
            olddata = pop!(junk)
            locals, ssavalues, sparams = olddata.locals, olddata.ssavalues, olddata.sparams
            exception_frames, last_reference = olddata.exception_frames, olddata.last_reference
            last_exception = olddata.last_exception
            callargs = olddata.callargs
            resize!(locals, length(src.slotflags))
            resize!(ssavalues, ng)
            # for check_isdefined to work properly, we need sparams to start out unassigned
            resize!(sparams, 0)
            empty!(exception_frames)
            empty!(last_reference)
            last_exception[] = nothing
        else
            locals = Vector{Union{Nothing,Some{Any}}}(undef, length(src.slotflags))
            ssavalues = Vector{Any}(undef, ng)
            sparams = Vector{Any}(undef, 0)
            exception_frames = Int[]
            last_reference = Dict{Symbol,Int}()
            callargs = Any[]
            last_exception = Ref{Any}(nothing)
        end
        for i = 1:meth_nargs
            last_reference[slotnames[i]::Symbol] = i
            if meth.isva && i == meth_nargs
                locals[i] = nargs < i ? Some{Any}(()) : (let i=i; Some{Any}(ntuple(k->argvals[i+k-1], nargs-i+1)); end)
                break
            end
            locals[i] = nargs >= i ? Some{Any}(argvals[i]) : Some{Any}(())
        end
        # add local variables initially undefined
        for i = (meth_nargs+1):length(slotnames)
            locals[i] = nothing
        end
    else
        src = framecode.src
        locals = Vector{Union{Nothing,Some{Any}}}(undef, length(src.slotflags))  # src.slotflags is concretely typed, unlike slotnames
        fill!(locals, nothing)
        ssavalues = Vector{Any}(undef, length(src.code))
        sparams = Any[]
        exception_frames = Int[]
        last_reference = Dict{Symbol,Int}()
        callargs = Any[]
        last_exception = Ref{Any}(nothing)
    end
    FrameData(locals, ssavalues, sparams, exception_frames, last_exception, caller_will_catch_err, last_reference, callargs)
end

"""
    frame = prepare_frame(framecode::FrameCode, frameargs, lenv)

Construct a new `Frame` for `framecode`, given lowered-code arguments `frameargs` and
static parameters `lenv`. See [`JuliaInterpreter.prepare_call`](@ref) for information about how to prepare the inputs.
"""
function prepare_frame(framecode::FrameCode, args::Vector{Any}, lenv::SimpleVector, caller_will_catch_err::Bool=false)
    framedata = prepare_framedata(framecode, args, caller_will_catch_err)
    resize!(framedata.sparams, length(lenv))
    # Add static parameters to environment
    for i = 1:length(lenv)
        T = lenv[i]
        isa(T, TypeVar) && continue  # only fill concrete types
        framedata.sparams[i] = T
    end
    return Frame(framecode, framedata)
end

function prepare_frame_caller(caller::Frame, framecode::FrameCode, args::Vector{Any}, lenv::SimpleVector)
    caller_will_catch_err = !isempty(caller.framedata.exception_frames) || caller.framedata.caller_will_catch_err
    caller.callee = frame = prepare_frame(framecode, args, lenv, caller_will_catch_err)
    frame.caller = caller
    return frame
end

"""
    frame = prepare_thunk(mod::Module, expr::Expr)

Prepare `expr` for evaluation in `mod`. `expr` should be a "straightforward" expression,
one that does not require special top-level handling (see [`JuliaInterpreter.split_expressions`](@ref)).
"""
function prepare_thunk(mod::Module, thunk::Expr, recursive::Bool=false)
    if isexpr(thunk, :thunk)
        framecode = FrameCode(mod, thunk.args[1])
    elseif isexpr(thunk, :error) || isexpr(thunk, :incomplete)
        error("lowering returned an error, ", thunk)
    elseif recursive
        thunk = Meta.lower(mod, thunk)
        if isa(thunk, Expr)
            # If on 2nd attempt to lower it's still an Expr, just evaluate it
            Core.eval(mod, thunk)
            return nothing
        end
        framecode = FrameCode(mod, thunk.args[1])
    else
        lwr = Meta.lower(mod, thunk)
        isa(lwr, Expr) && return prepare_thunk(mod, lwr, true)
        return nothing
    end
    return Frame(framecode, prepare_framedata(framecode, []))
end
prepare_thunk((mod, ex)::Tuple{Module,Expr}) = prepare_thunk(mod, ex)

"""
    modexs, docexprs = split_expressions(mod::Module, expr::Expr; extract_docexprs=false)

Break `expr` into a list `modexs` of sequential blocks. This is often needed when `expr`
needs to be evaluated at top level.

`modexs[i]` is a `(mod::Module, ex::Expr)` tuple, where `ex` is to be evaluated in `mod`.

# Toplevel evaluation

For code that defines new structs, new methods, or new macros, it can be important to evaluate
these expressions carefully:

    stack = Frame[]
    for modex in modexs    # or use `for (mod, ex) in modexs` to split the tuple
        frame = JuliaInterpreter.prepare_thunk(modex)
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end

The `while` loop here deserves some explanation. Occasionally, a frame may define new methods
(e.g., anonymous or local functions) and then call those methods. In such cases, running
the entire frame as a single block (e.g., with [`JuliaInterpreter.finish_and_return!`](@ref)
can trigger "method is too new..." errors. Instead, the approach above runs each frame,
but returns to the caller after any new method is defined. When this loop is running at
top level (e.g., in the REPL), this allows the world age to update and thus avoid
"method is too new..." errors.

Putting the above nested loop inside a function defeats its purpose, because inside a
compiled function the world age will not update. If necessary, use the following strategy:

    Core.eval(somemodule, Expr(:toplevel, quote
        body
    ))

where `body` contains the nested loop, plus any preparatory statements required to make the
necessary variables available at top level in `somemodule`.
"""
function split_expressions(mod::Module, expr::Expr; filename=nothing, kwargs...)
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
    return split_expressions!(modexs, docexprs, mod, expr; filename=filename, kwargs...)
end
split_expressions!(modexs, docexprs, mod::Module, ex::Expr; kwargs...) =
    split_expressions!(modexs, docexprs, Expr(:block), mod, ex; kwargs...)

function split_expressions!(modexs, docexprs, lex::Expr, mod::Module, ex::Expr; extract_docexprs=false, filename="toplevel")
    # lex is the expression we'll lower; it will accumulate LineNumberNodes and a
    # single top-level expression. We split blocks, module defs, etc.
    if ex.head == :toplevel || ex.head == :block
        split_expressions!(modexs, docexprs, lex, mod, ex.args; extract_docexprs=extract_docexprs, filename=filename)
    elseif ex.head == :module
        newname = ex.args[2]::Symbol
        if isdefined(mod, newname)
            newmod = getfield(mod, newname)
        else
            if (id = Base.identify_package(mod, String(newname))) !== nothing
                newmod = Base.root_module(id)
            else
                newmod = Core.eval(mod, :(module $newname end))
            end
        end
        split_expressions!(modexs, docexprs, lex, newmod, ex.args[3]; extract_docexprs=extract_docexprs, filename=filename)
    elseif extract_docexprs && is_doc_expr(ex) && length(ex.args) >= 4
        body = ex.args[4]
        if isa(body, Expr) && body.head != :call
            split_expressions!(modexs, docexprs, lex, mod, body; extract_docexprs=extract_docexprs, filename=filename)
        end
        docexs = get(docexprs, mod, nothing)
        if docexs === nothing
            docexs = docexprs[mod] = Expr[]
        end
        if isexpr(body, :module)
            # If it's a module expression, don't include the entire expression, just document the module itself.
            excopy = Expr(ex.head, ex.args[1], ex.args[2], ex.args[3])
            push!(excopy.args, body.args[2])
            if length(ex.args) > 4
                append!(excopy.args, ex.args[5:end])   # there should only be a 5th, but just for robustness
            end
            push!(docexs, excopy)
        else
            push!(docexs, ex)
        end
    else
        if isempty(lex.args)
            push!(modexs, (mod, copy(ex)))
        else
            push!(lex.args, ex)
            push!(modexs, (mod, copy(lex)))
            empty!(lex.args)
        end
    end
    return modexs, docexprs
end

function split_expressions!(frames, docexprs, lex, mod::Module, args::Vector{Any}; filename="toplevel", kwargs...)
    for a in args
        if isa(a, Expr)
            split_expressions!(frames, docexprs, lex, mod, a; filename=filename, kwargs...)
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

"""
    frame = enter_call_expr(expr; enter_generated=false)

Build a `Frame` ready to execute the expression `expr`. Set `enter_generated=true`
if you want to execute the generator of a `@generated` function, rather than the code that
would be created by the generator.

# Example

```jldoctest; setup=(using JuliaInterpreter; empty!(JuliaInterpreter.junk))
julia> mymethod(x) = x+1
mymethod (generic function with 1 method)

julia> JuliaInterpreter.enter_call_expr(:(\$mymethod(1)))
Frame for mymethod(x) in Main at none:1
  1* 1  1 ─ %1 = (+)(x, 1)
  2  1  └──      return %1
x = 1

julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 2 methods)

julia> a = [1.0, 2.0]
2-element Array{Float64,1}:
 1.0
 2.0

julia> JuliaInterpreter.enter_call_expr(:(\$mymethod(\$a)))
Frame for mymethod(x::Array{T,1}) where T in Main at none:1
  1* 1  1 ─     return 1
x = [1.0, 2.0]
T = Float64
```

See [`enter_call`](@ref) for a similar approach not based on expressions.
"""
function enter_call_expr(expr; enter_generated = false)
    empty!(junk)
    r = determine_method_for_expr(expr; enter_generated = enter_generated)
    if isa(r, Tuple)
        return prepare_frame(r[1:end-1]...)
    end
    nothing
end

"""
    frame = enter_call(f, args...; kwargs...)

Build a `Frame` ready to execute `f` with the specified positional and keyword arguments.

# Example

```jldoctest; setup=(using JuliaInterpreter; empty!(JuliaInterpreter.junk))
julia> mymethod(x) = x+1
mymethod (generic function with 1 method)

julia> JuliaInterpreter.enter_call(mymethod, 1)
Frame for mymethod(x) in Main at none:1
  1* 1  1 ─ %1 = (+)(x, 1)
  2  1  └──      return %1
x = 1

julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 2 methods)

julia> JuliaInterpreter.enter_call(mymethod, [1.0, 2.0])
Frame for mymethod(x::Array{T,1}) where T in Main at none:1
  1* 1  1 ─     return 1
x = [1.0, 2.0]
T = Float64
```

For a `@generated` function you can use `enter_call((f, true), args...; kwargs...)`
to execute the generator of a `@generated` function, rather than the code that
would be created by the generator.

See [`enter_call_expr`](@ref) for a similar approach based on expressions.
"""
function enter_call(@nospecialize(finfo), @nospecialize(args...); kwargs...)
    empty!(junk)
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
        return prepare_frame(r[1:end-1]...)
    end
    return nothing
end

# This is a version of InteractiveUtils.gen_call_with_extracted_types, except that is passes back the
# call expression for further processing.
function extract_args(__module__, ex0)
    if isa(ex0, Expr)
        if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
            return quote
                local arg1 = $(ex0.args[1])
                local args, kwargs = $separate_kwargs($(ex0.args[2:end]...))
                tuple(Core.kwfunc(arg1), kwargs, arg1, args...)
            end
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
    ex = Meta.lower(__module__, ex0)
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
        local theargs = $(esc(args))
        local frame = JuliaInterpreter.enter_call_expr(Expr(:call, theargs...))
        if frame === nothing
            eval(Expr(:call, map(QuoteNode, theargs)...))
        elseif shouldbreak(frame, 1)
            frame, BreakpointRef(frame.framecode, 1)
        else
            local ret = finish_and_return!(frame)
            # We deliberately return the top frame here; future debugging commands
            # via debug_command may alter the leaves, we want the top frame so we can
            # ultimately do `get_return`.
            isa(ret, BreakpointRef) ? (frame, ret) : ret
        end
    end
end
