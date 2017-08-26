__precompile__()
module ASTInterpreter2

using DebuggerFramework
using DebuggerFramework: FileLocInfo, BufferLocInfo
using Base.Meta

include("linearize.jl")
include("interpret.jl")

struct JuliaProgramCounter
    next_stmt::Int
end

struct JuliaStackFrame
    meth::Method
    code::CodeInfo
    locals::Vector{Nullable{Any}}
    ssavalues::Vector{Any}
    sparams::Vector{Any}
    pc::JuliaProgramCounter
    # A vector from names to the slotnumber of that name
    # for which a reference was last encountered.
    last_reference::Dict{Symbol, Int}
end
function JuliaStackFrame(frame::JuliaStackFrame, pc::JuliaProgramCounter)
    JuliaStackFrame(frame.meth, frame.code, frame.locals,
                    frame.ssavalues, frame.sparams, pc, frame.last_reference)
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
                    expr = exprtree.args[i]
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
        print(io, ") at ",frame.meth.file,":",frame.meth.line)
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

function JuliaStackFrame(meth::Method)
    JuliaStackFrame(meth, Vector{Nullable{Any}}(),
        Vector{Any}(), Vector{Any}(), Vector{Any}(),
        Dict{Symbol, Int}())
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

function determine_method_for_expr(interp, expr; enter_generated = false)
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
        code = get_source(method)
        # Get static parameters
        (ti, lenv) = ccall(:jl_match_method, Any, (Any, Any),
                            argtypes, sig)::SimpleVector
        if method.isstaged && !enter_generated
            # If we're stepping into a staged function, we need to use
            # the specialization, rather than stepping thorugh the
            # unspecialized method.
            code = Core.Inference.specialize_method(method, argtypes, lenv, false)
        else
            if method.isstaged
                args = map(_Typeof, args)
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

function prepare_locals(meth, code, argvals = ())
    code = deepcopy(code)
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
    JuliaStackFrame(meth, code, locals, ssavalues, sparams, JuliaProgramCounter(2), Dict{Symbol,Int}())
end


function enter_call_expr(interp, expr; enter_generated = false)
    r = determine_method_for_expr(interp, expr; enter_generated = enter_generated)
    if r !== nothing
        code, method, args, lenv = r
        frame = prepare_locals(method, code, args)
        # Add static parameters to environment
        for i = 1:length(lenv)
            frame.sparams[i] = lenv[i]
        end
        return frame
    end
    nothing
end

macro enter(arg)
    arg = expand(arg)
    @assert isa(arg, Expr) && arg.head == :call
    kws = collect(filter(x->isexpr(x,:kw),arg.args))
    if !isempty(kws)
      args = Expr(:tuple,:(Core.kwfunc($(args[1]))),
        Expr(:call,Base.vector_any,mapreduce(
          x->[QuoteNode(x.args[1]),x.args[2]],vcat,kws)...),
        map(x->isexpr(x,:parameters)?QuoteNode(x):x,
          filter(x->!isexpr(x,:kw),arg.args))...)
    else
      args = Expr(:tuple,
        map(x->isexpr(x,:parameters)?QuoteNode(x):x, arg.args)...)
    end
    quote
        theargs = $(esc(args))
        frame = ASTInterpreter2.enter_call_expr(nothing,Expr(:call,theargs...))
        DebuggerFramework.RunDebugger([frame])
    end
end

include("commands.jl")

end # module
