__precompile__()
module ASTInterpreter2

using DebuggerFramework
using DebuggerFramework: FileLocInfo, BufferLocInfo, Suppressed
using Base.Meta
using Base: LineEdit
import Base: +

export @enter, @make_stack

include("linearize.jl")
include("interpret.jl")

struct JuliaProgramCounter
    next_stmt::Int
end
+(x::JuliaProgramCounter, y::Integer) = JuliaProgramCounter(x.next_stmt+y)

struct JuliaStackFrame
    meth::Method
    code::CodeInfo
    locals::Vector{Nullable{Any}}
    ssavalues::Vector{Any}
    sparams::Vector{Any}
    exception_frames::Vector{Int}
    last_exception::Ref{Nullable{Any}}
    pc::JuliaProgramCounter
    # A vector from names to the slotnumber of that name
    # for which a reference was last encountered.
    last_reference::Dict{Symbol, Int}
    wrapper::Bool
    generator::Bool
    # Display options
    fullpath::Bool
end
function JuliaStackFrame(frame::JuliaStackFrame, pc::JuliaProgramCounter; wrapper = frame.wrapper, generator=frame.generator, fullpath=frame.fullpath)
    JuliaStackFrame(frame.meth, frame.code, frame.locals,
                    frame.ssavalues, frame.sparams,
                    frame.exception_frames, frame.last_exception,
                    pc, frame.last_reference, wrapper, generator,
                    fullpath)
end

is_loc_meta(expr, kind) = isexpr(expr, :meta) && length(expr.args) >= 1 && expr.args[1] === kind
function determine_line_and_file(frame, highlight::Int=0)
    file = frame.meth.file
    line = frame.meth.line
    foundline = false
    extra_locs = Any[]
    # Find a line number node previous to this expression
    if highlight !== 0 && !isempty(highlight)
        i = highlight
        while i >= 1
            expr = frame.code.code[i]
            if !foundline && isa(expr, LineNumberNode)
                line = expr.line
                foundline = true
            elseif !foundline && isexpr(expr, :line)
                line = expr.args[1]
                foundline = true
            elseif foundline && is_loc_meta(expr, :push_loc)
                file = expr.args[2]
                extra_locs = determine_line_and_file(frame, i-1)
                break
            elseif is_loc_meta(expr, :pop_loc)
                npops = 1
                while npops >= 1
                    i -= 1
                    expr = frame.code.code[i]
                    is_loc_meta(expr, :pop_loc) && (npops += 1)
                    is_loc_meta(expr, :push_loc) && (npops -= 1)
                end
            end
            i -= 1
        end
    end
    [extra_locs; (file, line)]
end

function DebuggerFramework.locdesc(frame::JuliaStackFrame, specslottypes = false)
    sprint() do io
        slottypes = frame.code.slottypes
        argnames = frame.code.slotnames[2:frame.meth.nargs]
        spectypes = specslottypes && (slottypes != nothing) ?
            slottypes[2:frame.meth.nargs] : Any[Any for i=1:length(argnames)]
        print(io, frame.meth.name,'(')
        first = true
        for (argname, argT) in zip(argnames, spectypes)
            first || print(io, ", ")
            first = false
            print(io, argname)
            !(argT === Any) && print(io, "::", argT)
        end
        print(io, ") at ",
            frame.fullpath ? frame.meth.file :
            basename(String(frame.meth.file)),
            ":",frame.meth.line)
    end
end

function DebuggerFramework.print_locals(io::IO, frame::JuliaStackFrame)
    for i = 1:length(frame.locals)
        if !isnull(frame.locals[i])
            # #self# is only interesting if it has values inside of it. We already know
            # which function we're in otherwise.
            val = get(frame.locals[i])
            if frame.code.slotnames[i] == Symbol("#self#") && (isa(val, Type) || sizeof(val) == 0)
                continue
            end
            DebuggerFramework.print_var(io, frame.code.slotnames[i], frame.locals[i], nothing)
        end
    end
    for i = 1:length(frame.sparams)
        DebuggerFramework.print_var(io, frame.meth.sparam_syms[i], Nullable{Any}(frame.sparams[i]), nothing)
    end
end


const SEARCH_PATH = []
__init__() = append!(SEARCH_PATH,[joinpath(JULIA_HOME,"../share/julia/base/"),
    joinpath(JULIA_HOME,"../include/")])
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
    file, line = determine_line_and_file(frame, frame.pc.next_stmt)[end]
    loc_for_fname(file, line, frame.meth.line)
end

function lookup_var_if_var(frame, x)
    if isa(x, Union{SSAValue, GlobalRef, SlotNumber}) || isexpr(x, :static_parameter) || isexpr(x, :the_exception) || isexpr(x, :boundscheck)
        return lookup_var(frame, x)
    end
    x
end

function DebuggerFramework.eval_code(state, frame::JuliaStackFrame, command)
    expr = Base.parse_input_line(command)
    local_vars = Any[]
    local_vals = Any[]
    for i = 1:length(frame.locals)
        if !isnull(frame.locals[i])
            push!(local_vars, frame.code.slotnames[i])
            push!(local_vals, QuoteNode(get(frame.locals[i])))
        end
    end
    for i = 1:length(frame.sparams)
        push!(local_vars, frame.meth.sparam_syms[i])
        push!(local_vals, QuoteNode(frame.sparams[i]))
    end
    res = gensym()
    eval_expr = Expr(:let, Expr(:block,
        Expr(:(=), res, expr),
        Expr(:tuple, res, Expr(:tuple, local_vars...))),
    map(x->Expr(:(=), x...), zip(local_vars, local_vals))...)
    eval_res, res = eval(frame.meth.module, eval_expr)
    j = 1
    for i = 1:length(frame.locals)
        if !isnull(frame.locals[i])
            frame.locals[i] = Nullable{Any}(res[j])
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
    expr = pc_expr(frame, frame.pc)
    isa(expr, Expr) && (expr = copy(expr))
    if isexpr(expr, :(=))
        expr = expr.args[2]
    end
    if isexpr(expr, :call) || isexpr(expr, :return)
        expr.args = map(var->maybe_quote(lookup_var_if_var(frame, var)), expr.args)
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
        prompt_suffix = (state.repl.envcolors ? Base.input_color : repl.input_color),
        complete = Base.REPL.REPLCompletionProvider(),
        on_enter = Base.REPL.return_callback)
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
        Base.REPL.print_response(state.repl, ok ? result : result[1], ok ? nothing : result[2], true, true)
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
    julia_prompt.keymap_dict = LineEdit.keymap([Base.REPL.mode_keymap(state.main_mode);state.standard_keymap])
    state.language_modes[:julia] = julia_prompt
    return julia_prompt
end

function JuliaStackFrame(meth::Method)
    JuliaStackFrame(meth, Vector{Nullable{Any}}(),
        Vector{Any}(), Vector{Any}(), Vector{Any}(),
        Dict{Symbol, Int}(), false, false, true)
end

function DebuggerFramework.debug(meth::Method, args...)
    stack = [JuliaStackFrame(meth)]
    DebuggerFramework.RunDebugger(stack)
end

function to_function(x)
    if isa(x, Function) || isa(x, Core.IntrinsicFunction)
        x
    elseif isa(x, GlobalRef)
        eval(x)
    else
        x
    end
end

_Typeof(x) = isa(x,Type) ? Type{x} : typeof(x)

function is_function_def(ex)
    (isa(ex,Expr) && ex.head == :(=) && isexpr(ex.args[1],:call)) ||
    isexpr(ex,:function)
end

function is_generated(meth)
    if VERSION < v"0.7-"
        meth.isstaged
    else
        isdefined(meth, :generator)
    end
end

function determine_method_for_expr(expr; enter_generated = false)
    f = to_function(expr.args[1])
    allargs = expr.args
    # Extract keyword args
    local kwargs = Expr(:parameters)
    if length(allargs) > 1 && isexpr(allargs[2], :parameters)
        kwargs = splice!(allargs, 2)
    end
    if !isempty(kwargs.args)
        of = f
        f = Core.kwfunc(f)
        allargs = [f,reduce(vcat,Any[[ex.args[1];ex.args[2]] for ex in kwargs.args]),of,
            allargs[2:end]...]
    elseif f === Core._apply
        f = to_function(allargs[2])
        allargs = Base.append_any((allargs[2],), allargs[3:end]...)
    end
    # Can happen for thunks created by generated functions
    if !isa(f, Core.Builtin) && !isa(f, Core.IntrinsicFunction)
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
        if VERSION < v"0.7-"
            (ti, lenv) = ccall(:jl_match_method, Any, (Any, Any),
                                argtypes, sig)::SimpleVector
        else
            (ti, lenv) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                                argtypes, sig)::SimpleVector
        end
        if is_generated(method) && !enter_generated
            # If we're stepping into a staged function, we need to use
            # the specialization, rather than stepping thorugh the
            # unspecialized method.
            code = Core.Inference.get_staged(Core.Inference.code_for_method(method, argtypes, lenv, typemax(UInt), false))
        else
            if is_generated(method)
                args = map(_Typeof, args)
                if VERSION < v"0.7-"
                    code = get_source(method)
                else
                    code = get_source(method.generator.gen)
                end
            else
                code = get_source(method)
            end
        end
        return code, method, args, lenv
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

function copy_codeinfo(code::CodeInfo)
    old_code = code.code
    code.code = UInt8[]
    new_codeinfo = deepcopy(code)
    new_codeinfo.code = old_code
    code.code = old_code
    new_codeinfo
end

function prepare_locals(meth, code, argvals = (), generator = false)
    code = copy_codeinfo(code)
    linearize!(code)
    # Construct the environment from the arguments
    argnames = code.slotnames[1:meth.nargs]
    locals = Array{Nullable{Any}}(length(code.slotflags))
    ng = isa(code.ssavaluetypes, Int) ? code.ssavaluetypes : length(code.ssavaluetypes)
    ssavalues = Array{Any}(ng)
    sparams = Array{Any}(length(meth.sparam_syms))
    for i = 1:meth.nargs
        if meth.isva && i == length(argnames)
            locals[i] = length(argvals) >= i ? tuple(argvals[i:end]...) : Nullable{Any}(())
            break
        end
        locals[i] = length(argvals) >= i ? Nullable{Any}(argvals[i]) : Nullable{Any}()
    end
    # add local variables initially undefined
    for i = (meth.nargs+1):length(code.slotnames)
        locals[i] = Nullable{Any}()
    end
    JuliaStackFrame(meth, code, locals, ssavalues, sparams,  Int[], Nullable{Any}(),
        JuliaProgramCounter(2), Dict{Symbol,Int}(), false, generator,
        true)
end


function enter_call_expr(expr; enter_generated = false)
    r = determine_method_for_expr(expr; enter_generated = enter_generated)
    if r !== nothing
        code, method, args, lenv = r
        frame = prepare_locals(method, code, args, enter_generated)
        # Add static parameters to environment
        for i = 1:length(lenv)
            frame.sparams[i] = lenv[i]
        end
        return frame
    end
    nothing
end

function maybe_step_through_wrapper!(stack)
    last = stack[1].code.code[end-1]
    isexpr(last, :(=)) && (last = last.args[2])
    is_kw = startswith(String(Base.unwrap_unionall(stack[1].meth.sig).parameters[1].name.name), "#kw")
    if is_kw || isexpr(last, :call) && any(x->x==SlotNumber(1), last.args)
        # If the last expr calls #self# or passes it to an implemetnation method,
        # this is a wrapper function that we might want to step though
        frame = stack[1]
        pc = frame.pc
        while pc != JuliaProgramCounter(length(frame.code.code)-1)
            pc = next_call!(frame, pc)
        end
        stack[1] = JuliaStackFrame(frame, pc; wrapper=true)
        unshift!(stack, enter_call_expr(Expr(:call, map(x->lookup_var_if_var(frame, x), last.args)...)))
        return maybe_step_through_wrapper!(stack)
    end
    stack
end

lower(mod, arg) = VERSION < v"0.7-" ? expand(arg) : Meta.lower(mod, arg)

function _make_stack(mod, arg)
    arg = lower(mod, arg)
    @assert isa(arg, Expr) && arg.head == :call
    kws = collect(filter(x->isexpr(x,:kw),arg.args))
    if !isempty(kws)
      args = Expr(:tuple,:(Core.kwfunc($(args[1]))),
        Expr(:call,Base.vector_any,mapreduce(
          x->[QuoteNode(x.args[1]),x.args[2]],vcat,kws)...),
        map(x->isexpr(x, :parameters) ? QuoteNode(x) : x,
          filter(x->!isexpr(x, :kw),arg.args))...)
    else
      args = Expr(:tuple,
        map(x->isexpr(x,:parameters) ? QuoteNode(x) : x, arg.args)...)
    end
    quote
        theargs = $(esc(args))
        stack = [ASTInterpreter2.enter_call_expr(Expr(:call,theargs...))]
        ASTInterpreter2.maybe_step_through_wrapper!(stack)
        stack[1] = ASTInterpreter2.JuliaStackFrame(stack[1], ASTInterpreter2.maybe_next_call!(stack[1]))
        stack
    end
end

macro make_stack(arg)
     if VERSION < v"0.7-"
         __module__ = current_module()
     end
    _make_stack(__module__, arg)
end

macro enter(arg)
    if VERSION < v"0.7-"
        __module__ = current_module()
    end
    quote
        let stack = $(_make_stack(__module__,arg))
            DebuggerFramework.RunDebugger(stack)
        end
    end
end

include("commands.jl")

end # module
