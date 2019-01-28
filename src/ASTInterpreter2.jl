module ASTInterpreter2

using DebuggerFramework
using DebuggerFramework: FileLocInfo, BufferLocInfo, Suppressed
using Base.Meta
using REPL.LineEdit
using REPL
import Base: +, convert, isless
using Core: CodeInfo, SSAValue, SlotNumber, TypeMapEntry, SimpleVector, LineInfoNode, GotoNode, Slot,
            GeneratedFunctionStub, MethodInstance
using Markdown

export @enter, @make_stack, @interpret, Compiled, JuliaStackFrame

"""
`Compiled` is a trait indicating that any `:call` expressions should be evaluated
using Julia's normal compiled-code evaluation. The alternative is to pass `stack=JuliaStackFrame[]`,
which will cause all calls to be evaluated via the interpreter.
"""
struct Compiled end

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

const junk = JuliaStackFrame[]      # to allow re-use of allocated memory (this is otherwise a bottleneck)

include("localmethtable.jl")
include("interpret.jl")
include("builtins.jl")

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

function DebuggerFramework.locdesc(frame::JuliaStackFrame, specslottypes = false)
    sprint() do io
        if frame.code.scope isa Method
            meth = frame.code.scope
            argnames = frame.code.code.slotnames[2:meth.nargs]
            spectypes = Any[Any for i=1:length(argnames)]
            print(io, meth.name,'(')
            first = true
            for (argname, argT) in zip(argnames, spectypes)
                first || print(io, ", ")
                first = false
                print(io, argname)
                !(argT === Any) && print(io, "::", argT)
            end
            print(io, ") at ",
                frame.code.fullpath ? meth.file :
                basename(String(meth.file)),
                ":",meth.line)
        else
            println("not yet implemented")
        end
    end
end

function DebuggerFramework.print_locals(io::IO, frame::JuliaStackFrame)
    for i = 1:length(frame.locals)
        if !isa(frame.locals[i], Nothing)
            # #self# is only interesting if it has values inside of it. We already know
            # which function we're in otherwise.
            val = something(frame.locals[i])
            if frame.code.code.slotnames[i] == Symbol("#self#") && (isa(val, Type) || sizeof(val) == 0)
                continue
            end
            DebuggerFramework.print_var(io, frame.code.code.slotnames[i], frame.locals[i], nothing)
        end
    end
    if frame.code.scope isa Method
        for i = 1:length(frame.sparams)
            DebuggerFramework.print_var(io, frame.code.scope.sparam_syms[i], frame.sparams[i], nothing)
        end
    end
end


const SEARCH_PATH = []
function __init__()
    append!(SEARCH_PATH,[joinpath(Sys.BINDIR,"../share/julia/base/"),
            joinpath(Sys.BINDIR,"../include/")])
    return nothing
end

function loc_for_fname(file, line, defline)
    if startswith(string(file),"REPL[")
        hist_idx = parse(Int,string(file)[6:end-1])
        isdefined(Base, :active_repl) || return nothing, ""
        hp = Base.active_repl.interface.modes[1].hist
        return BufferLocInfo(hp.history[hp.start_idx+hist_idx], line, 0, defline)
    else
        for path in SEARCH_PATH
            fullpath = joinpath(path,string(file))
            if isfile(fullpath)
                return FileLocInfo(fullpath, line, 0, defline)
            end
        end
    end
    return nothing
end

function DebuggerFramework.locinfo(frame::JuliaStackFrame)
    if frame.code.scope isa Method
        meth = frame.code.scope
        loc_for_fname(meth.file, location(frame), meth.line)
    else
        println("not yet implemented")
    end
end

function DebuggerFramework.eval_code(state, frame::JuliaStackFrame, command)
    expr = Base.parse_input_line(command)
    if isexpr(expr, :toplevel)
        expr = expr.args[end]
    end
    local_vars = Any[]
    local_vals = Any[]
    for i = 1:length(frame.locals)
        if !isa(frame.locals[i], Nothing)
            push!(local_vars, frame.code.code.slotnames[i])
            push!(local_vals, QuoteNode(something(frame.locals[i])))
        end
    end
    ismeth = frame.code.scope isa Method
    for i = 1:length(frame.sparams)
        ismeth && push!(local_vars, frame.code.scope.sparam_syms[i])
        push!(local_vals, QuoteNode(frame.sparams[i]))
    end
    res = gensym()
    eval_expr = Expr(:let,
        Expr(:block, map(x->Expr(:(=), x...), zip(local_vars, local_vals))...),
        Expr(:block,
            Expr(:(=), res, expr),
            Expr(:tuple, res, Expr(:tuple, local_vars...))
        ))
    eval_res, res = Core.eval(moduleof(frame), eval_expr)
    j = 1
    for i = 1:length(frame.locals)
        if !isa(frame.locals[i], Nothing)
            frame.locals[i] = Some{Any}(res[j])
            j += 1
        end
    end
    for i = 1:length(frame.sparams)
        frame.sparams[i] = res[j]
        j += 1
    end
    eval_res
end

function maybe_quote(x)
    (isa(x, Expr) || isa(x, Symbol)) ? QuoteNode(x) : x
end

function DebuggerFramework.print_next_state(io::IO, state, frame::JuliaStackFrame)
    print(io, "About to run: ")
    expr = plain(pc_expr(frame, frame.pc[]))
    isa(expr, Expr) && (expr = copy(expr))
    if isexpr(expr, :(=))
        expr = expr.args[2]
    end
    if isexpr(expr, :call) || isexpr(expr, :return)
        expr.args = map(var->maybe_quote(@eval_rhs(true, frame, var, frame.pc[])), expr.args)
    end
    if isa(expr, Expr)
        for (i, arg) in enumerate(expr.args)
            try
                nbytes = length(repr(arg))
                if nbytes > max(40, div(200, length(expr.args)))
                    expr.args[i] = Suppressed("$nbytes bytes of output")
                end
            catch
                expr.args[i] = Suppressed("printing error")
            end
        end
    end
    print(io, expr)
    println(io)
end

const all_commands = ("q", "s", "si", "finish", "bt", "loc", "ind", "shadow",
    "up", "down", "nc", "n", "se")

function DebuggerFramework.language_specific_prompt(state, frame::JuliaStackFrame)
    if haskey(state.language_modes, :julia)
        return state.language_modes[:julia]
    end
    julia_prompt = LineEdit.Prompt(DebuggerFramework.promptname(state.level, "julia");
        # Copy colors from the prompt object
        prompt_prefix = state.repl.prompt_color,
        prompt_suffix = (state.repl.envcolors ? Base.input_color : state.repl.input_color),
        complete = REPL.REPLCompletionProvider(),
        on_enter = REPL.return_callback)
    # 0.7 compat
    if isdefined(state.main_mode, :repl)
        julia_prompt.repl = state.main_mode.repl
    end
    julia_prompt.hist = state.main_mode.hist
    julia_prompt.hist.mode_mapping[:julia] = julia_prompt

    julia_prompt.on_done = (s,buf,ok)->begin
        if !ok
            LineEdit.transition(s, :abort)
            return false
        end
        xbuf = copy(buf)
        command = String(take!(buf))
        ok, result = DebuggerFramework.eval_code(state, command)
        REPL.print_response(state.repl, ok ? result : result[1], ok ? nothing : result[2], true, true)
        println(state.repl.t)

        if !ok
            # Convenience hack. We'll see if this is more useful or annoying
            for c in all_commands
                !startswith(command, c) && continue
                LineEdit.transition(s, state.main_mode)
                LineEdit.state(s, state.main_mode).input_buffer = xbuf
                break
            end
        end
        LineEdit.reset_state(s)
    end
    julia_prompt.keymap_dict = LineEdit.keymap([REPL.mode_keymap(state.main_mode);state.standard_keymap])
    state.language_modes[:julia] = julia_prompt
    return julia_prompt
end

function DebuggerFramework.debug(meth::Method, args...)
    stack = [enter_call(meth, args...)]
    DebuggerFramework.RunDebugger(stack)
end

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

```jldoctest; setup=(using ASTInterpreter2; empty!(ASTInterpreter2.junk))
julia> mymethod(x) = 1
mymethod (generic function with 1 method)

julia> mymethod(x, y; verbose=false) = nothing
mymethod (generic function with 2 methods)

julia> ASTInterpreter2.prepare_args(mymethod, [mymethod, 15], ())
(mymethod, Any[mymethod, 15])

julia> ASTInterpreter2.prepare_args(mymethod, [mymethod, 1, 2], [:verbose=>true])
(getfield( Symbol("#kw##mymethod"))(), Any[#kw##mymethod(), Any[:verbose, true], mymethod, 1, 2])
```
"""
function prepare_args(f, allargs, kwargs)
    if !isempty(kwargs)
        of = f
        f = Core.kwfunc(f)
        allargs = [f,namedtuple(kwargs),allargs...]
    elseif f === Core._apply
        f = to_function(allargs[2])
        allargs = Base.append_any((allargs[2],), allargs[3:end]...)
    end
    return f, allargs
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

```jldoctest; setup=(using ASTInterpreter2; empty!(ASTInterpreter2.junk))
julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 1 method)

julia> framecode, frameargs, lenv, argtypes = ASTInterpreter2.prepare_call(mymethod, [mymethod, [1.0,2.0]]);

julia> framecode
ASTInterpreter2.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(
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
function prepare_call(f, allargs; enter_generated = false)
    args = allargs[2:end]
    argtypes = Tuple{map(_Typeof,args)...}
    method = try
        which(f, argtypes)
    catch err
        @show typeof(f)
        println(f)
        println(argtypes)
        rethrow(err)
    end
    argtypes = Tuple{_Typeof(f), argtypes.parameters...}
    args = allargs
    sig = method.sig
    isa(method, TypeMapEntry) && (method = method.func)
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
    framecode, frameargs, lenv, argtypes = determine_method_for_expr(expr; enter_generated = false)

Prepare all the information needed to execute a particular `:call` expression `expr`.
For example, try `ASTInterpreter2.determine_method_for_expr(:(sum([1,2])))`.
See [`ASTInterpreter2.prepare_call`](@ref) for information about the outputs.
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
    # Can happen for thunks created by generated functions
    if !isa(f, Core.Builtin) && !isa(f, Core.IntrinsicFunction)
        return prepare_call(f, allargs; enter_generated=enter_generated)
    end
    nothing
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
            ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), newcode, i-1, val===nothing ? val : copy(val))
        end
    end
    return newcode
end

const calllike = Set([:call, :struct_type])

function extract_inner_call!(stmt, idx, once::Bool=false)
    isa(stmt, Expr) || return nothing
    once |= stmt.head ∈ calllike
    for (i, a) in enumerate(stmt.args)
        isa(a, Expr) || continue
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

function lookup_global_refs!(ex::Expr)
    for (i, a) in enumerate(ex.args)
        if isa(a, GlobalRef)
            r = getfield(a.mod, a.name)
            ex.args[i] = QuoteNode(r)
        elseif isa(a, Expr)
            lookup_global_refs!(a)
        end
    end
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
            if stmt.head == :call && isa(stmt.args[1], GlobalRef)
                # Special handling of cglobal, which requires constants for its arguments
                r = stmt.args[1]::GlobalRef
                f = getfield(r.mod, r.name)
                if f === Base.cglobal
                    code.code[i] = QuoteNode(Core.eval(mod, stmt))
                else
                    lookup_global_refs!(stmt)
                end
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
    for (i, stmt) in enumerate(new_code)
        if isa(stmt, GotoNode)
            new_code[i] = GotoNode(ssalookup[stmt.label])
        elseif isa(stmt, SSAValue)
            new_code[i] = SSAValue(ssalookup[stmt.id])
        elseif isa(stmt, NewSSAValue)
            new_code[i] = SSAValue(stmt.id)
        elseif isa(stmt, Expr)
            replace_ssa!(stmt, ssalookup)
            if stmt.head == :gotoifnot && isa(stmt.args[2], Int)
                stmt.args[2] = ssalookup[stmt.args[2]]
            end
        end
    end
    code.ssavaluetypes = length(new_code)
    return code
end

plain(stmt) = stmt

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
            # for check_isdefined to work properly, we need locals and sparams to start out unassigned
            resize!(resize!(locals, 0), length(code.slotflags))
            resize!(ssavalues, ng)
            resize!(resize!(sparams, 0), length(meth.sparam_syms))
            empty!(exception_frames)
            empty!(last_reference)
            last_exception[] = nothing
            pc[] = JuliaProgramCounter(1)
        else
            locals = Vector{Union{Nothing,Some{Any}}}(undef, length(code.slotflags))
            ssavalues = Vector{Any}(undef, ng)
            sparams = Vector{Any}(undef, length(meth.sparam_syms))
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
static parameters `lenv`. See [`ASTInterpreter2.prepare_call`](@ref) for information about how to prepare the inputs.
"""
function build_frame(framecode, args, lenv)
    frame = prepare_locals(framecode, args)
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

```jldoctest; setup=(using ASTInterpreter2; empty!(ASTInterpreter2.junk))
julia> mymethod(x) = x+1
mymethod (generic function with 1 method)

julia> ASTInterpreter2.enter_call_expr(:(\$mymethod(1)))
JuliaStackFrame(ASTInterpreter2.JuliaFrameCode(mymethod(x) in Main at none:1, CodeInfo(
1 ─ %1 = (\$(QuoteNode(+)))(x, 1)
└──      return %1
), Core.TypeMapEntry[#undef, #undef], BitSet([1]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some(1)], Any[#undef, #undef], Any[], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{ASTInterpreter2.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])

julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 2 methods)

julia> a = [1.0, 2.0]
2-element Array{Float64,1}:
 1.0
 2.0

julia> ASTInterpreter2.enter_call_expr(:(\$mymethod(\$a)))
JuliaStackFrame(ASTInterpreter2.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(
1 ─     return 1
), Core.TypeMapEntry[#undef], BitSet([]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some([1.0, 2.0])], Any[#undef], Any[Float64], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{ASTInterpreter2.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])
```

See [`enter_call`](@ref) for a similar approach not based on expressions.
"""
function enter_call_expr(expr; enter_generated = false)
    r = determine_method_for_expr(expr; enter_generated = enter_generated)
    if r !== nothing
        return build_frame(r[1:end-1]...)
    end
    nothing
end

"""
    frame = enter_call(f, args...; kwargs...)

Build a `JuliaStackFrame` ready to execute `f` with the specified positional and keyword arguments.

# Example

```jldoctest; setup=(using ASTInterpreter2; empty!(ASTInterpreter2.junk))
julia> mymethod(x) = x+1
mymethod (generic function with 1 method)

julia> ASTInterpreter2.enter_call(mymethod, 1)
JuliaStackFrame(ASTInterpreter2.JuliaFrameCode(mymethod(x) in Main at none:1, CodeInfo(
1 ─ %1 = ($(QuoteNode(+)))(x, 1)
└──      return %1
), Core.TypeMapEntry[#undef, #undef], BitSet([1]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some(1)], Any[#undef, #undef], Any[], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{ASTInterpreter2.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])

julia> mymethod(x::Vector{T}) where T = 1
mymethod (generic function with 2 methods)

julia> ASTInterpreter2.enter_call(mymethod, [1.0, 2.0])
JuliaStackFrame(ASTInterpreter2.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(
1 ─     return 1
), Core.TypeMapEntry[#undef], BitSet([]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some([1.0, 2.0])], Any[#undef], Any[Float64], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{ASTInterpreter2.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])
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
    if r !== nothing
        return build_frame(r[1:end-1]...)
    end
    return nothing
end

function maybe_step_through_wrapper!(stack)
    length(stack[1].code.code.code) < 2 && return stack
    last = plain(stack[1].code.code.code[end-1])
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
        newcall = Expr(:call, map(x->@eval_rhs(true, frame, x, pc), last.args)...)
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
        stack = [ASTInterpreter2.enter_call_expr(Expr(:call,theargs...))]
        ASTInterpreter2.maybe_step_through_wrapper!(stack)
        stack[1] = ASTInterpreter2.JuliaStackFrame(stack[1], ASTInterpreter2.maybe_next_call!(Compiled(), stack[1]))
        stack
    end
end

macro make_stack(arg)
    _make_stack(__module__, arg)
end

macro enter(arg)
    quote
        let stack = $(_make_stack(__module__,arg))
            DebuggerFramework.RunDebugger(stack)
        end
    end
end

"""
    @interpret f(args; kwargs...)

Evaluate `f` on the specified arguments using the interpreter.

# Example

```jldoctest; setup=(using ASTInterpreter2; empty!(ASTInterpreter2.junk))
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
        frame = ASTInterpreter2.enter_call_expr(Expr(:call,theargs...))
        empty!(framedict)  # start fresh each time; kind of like bumping the world age at the REPL prompt
        empty!(genframedict)
        finish_and_return!(stack, frame)
    end
end

include("commands.jl")

end # module
