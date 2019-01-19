module ASTInterpreter2

using DebuggerFramework
using DebuggerFramework: FileLocInfo, BufferLocInfo, Suppressed
using Base.Meta
using REPL.LineEdit
using REPL
import Base: +, deepcopy_internal
using Core: CodeInfo, SSAValue, SlotNumber, TypeMapEntry, SimpleVector, LineInfoNode, GotoNode, Slot,
            GeneratedFunctionStub, MethodInstance
using Markdown

export @enter, @make_stack, Compiled, JuliaStackFrame

"""
`Compiled` is a trait indicating that any `:call` expressions should be evaluated
using Julia's normal compiled-code evaluation. The alternative is to pass `stack=JuliaStackFrame[]`,
which will cause all calls to be evaluated via the interpreter.
"""
struct Compiled end

Base.push!(::Compiled, frame) = frame
Base.pop!(::Compiled) = nothing

struct JuliaProgramCounter
    next_stmt::Int
end
+(x::JuliaProgramCounter, y::Integer) = JuliaProgramCounter(x.next_stmt+y)

Base.show(io::IO, pc::JuliaProgramCounter) = print(io, "JuliaProgramCounter(", pc.next_stmt, ')')

"""
`JuliaFrameCode` holds static information about a method or toplevel code.
One `JuliaFrameCode` can be shared by many `JuliaFrameState` calling frames.
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

"""
`JuliaStackFrame` represents the execution state in a particular call frame.
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
end

function JuliaStackFrame(frame::JuliaStackFrame, pc::JuliaProgramCounter)
    JuliaStackFrame(frame.code, frame.locals,
                    frame.ssavalues, frame.sparams,
                    frame.exception_frames, frame.last_exception,
                    Ref(pc), frame.last_reference)
end
function JuliaStackFrame(framecode::JuliaFrameCode, frame::JuliaStackFrame, pc::JuliaProgramCounter)
    JuliaStackFrame(framecode, frame.locals,
                    frame.ssavalues, frame.sparams,
                    frame.exception_frames, frame.last_exception,
                    Ref(pc), frame.last_reference)
end

const framedict = Dict{Tuple{Method,Bool},JuliaFrameCode}()  # essentially a method table for lowered code
const junk = JuliaStackFrame[]      # to allow re-use of allocated memory (this is otherwise a bottleneck)

include("localmethtable.jl")
include("interpret.jl")
include("builtins.jl")

moduleof(frame) = isa(frame.code.scope, Method) ? frame.code.scope.module : frame.code.scope
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

kwpair(ex::Expr) = [ex.args[1]; ex.args[2]]
kwpair(pr::Pair) = [pr.first; pr.second]

function prepare_args(f, allargs, kwargs)
    if !isempty(kwargs)
        of = f
        f = Core.kwfunc(f)
        allargs = [f,reduce(vcat,Any[kwpair(ex) for ex in kwargs]),of,
            allargs[2:end]...]
    elseif f === Core._apply
        f = to_function(allargs[2])
        allargs = Base.append_any((allargs[2],), allargs[3:end]...)
    end
    return f, allargs
end

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
    framecode = get(framedict, (method, enter_generated), nothing)
    if framecode === nothing
        if is_generated(method) && !enter_generated
            # If we're stepping into a staged function, we need to use
            # the specialization, rather than stepping thorugh the
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
        code = optimize!(copy_codeinfo(code))
        used = find_used(code)
        methodtables = Vector{TypeMapEntry}(undef, length(code.code))
        framecode = JuliaFrameCode(method, code, methodtables, used, false, generator, true)
        framedict[(method, generator)] = framecode
    end
    return framecode, args, lenv, argtypes
end

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

function Base.deepcopy_internal(x::LineInfoNode, stackdict::IdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    deeper(x) = deepcopy_internal(x, stackdict)
    stackdict[x] = LineInfoNode(x.mod, deeper(x.method),
        deeper(x.file), deeper(x.line), deeper(x.inlined_at))
end

function copy_codeinfo(code::CodeInfo)
    old_code = code.code
    code.code = UInt8[]
    new_codeinfo = deepcopy(code)
    new_codeinfo.code = old_code
    code.code = old_code
    new_codeinfo
end

optimize!(framecode) = framecode
plain(stmt) = stmt

function prepare_locals(framecode, argvals::Vector{Any}, generator = false)
    meth, code = framecode.scope::Method, framecode.code
    ssavt = code.ssavaluetypes
    ng = isa(ssavt, Int) ? ssavt : length(ssavt::Vector{Any})
    nargs = length(argvals)
    if !isempty(junk)
        oldframe = pop!(junk)
        locals, ssavalues, sparams = oldframe.locals, oldframe.ssavalues, oldframe.sparams
        exception_frames, last_reference = oldframe.exception_frames, oldframe.last_reference
        resize!(locals, length(code.slotflags))
        resize!(ssavalues, ng)
        resize!(sparams, length(meth.sparam_syms))
        empty!(exception_frames)
        empty!(last_reference)
    else
        locals = Vector{Union{Nothing,Some{Any}}}(undef, length(code.slotflags))
        ssavalues = Vector{Any}(undef, ng)
        sparams = Vector{Any}(undef, length(meth.sparam_syms))
        exception_frames = Int[]
        last_reference = Dict{Symbol,Int}()
    end
    for i = 1:meth.nargs
        if meth.isva && i == meth.nargs
            locals[i] = nargs >= i ? Some{Any}(tuple(argvals[i:end]...)) : Some{Any}(())
            break
        end
        locals[i] = nargs >= i ? Some{Any}(argvals[i]) : Some{Any}(())
    end
    # add local variables initially undefined
    for i = (meth.nargs+1):length(code.slotnames)
        locals[i] = nothing
    end
    JuliaStackFrame(framecode, locals, ssavalues, sparams, exception_frames, Ref{Any}(nothing),
                    Ref(JuliaProgramCounter(1)), last_reference)
end

function build_frame(framecode, args, lenv; enter_generated=false)
    frame = prepare_locals(framecode, args, enter_generated)
    # Add static parameters to environment
    for i = 1:length(lenv)
        frame.sparams[i] = lenv[i]
    end
    return frame
end

function enter_call_expr(expr; enter_generated = false)
    r = determine_method_for_expr(expr; enter_generated = enter_generated)
    if r !== nothing
        return build_frame(r[1:end-1]...; enter_generated=enter_generated)
    end
    nothing
end

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
        return build_frame(r[1:end-1]...; enter_generated=enter_generated)
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
        # If the last expr calls #self# or passes it to an implemetnation method,
        # this is a wrapper function that we might want to step though
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

include("commands.jl")

end # module
