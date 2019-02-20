# Implements a simple interpreter for julia's lowered AST

getlhs(pc) = SSAValue(pc.next_stmt)

isassign(fr) = isassign(fr, fr.pc[])
isassign(fr, pc) = (pc.next_stmt in fr.code.used)

lookup_var(frame, val::SSAValue) = frame.ssavalues[val.id]
lookup_var(frame, ref::GlobalRef) = getfield(ref.mod, ref.name)
function lookup_var(frame, slot::SlotNumber)
    val = frame.locals[slot.id]
    val !== nothing && return val.value
    error("slot ", slot, " with name ", frame.code.code.slotnames[slot.id], " not assigned")
end

function lookup_expr(frame, e::Expr)
    head = e.head
    head == :the_exception && return frame.last_exception[]
    head == :static_parameter && return frame.sparams[e.args[1]::Int]
    head == :boundscheck && length(e.args) == 0 && return true
    error("invalid lookup expr ", e)
end

"""
    rhs = @lookup(frame, node)
    rhs = @lookup(mod, frame, node)

This macro looks up previously-computed values referenced as SSAValues, SlotNumbers,
GlobalRefs, QuoteNode, sparam or exception reference expression.
It will also lookup symbols in `moduleof(frame)`; this can be supplied ahead-of-time via
the 3-argument version.
If none of the above apply, the value of `node` will be returned.
"""
macro lookup(args...)
    length(args) == 2 || length(args) == 3 || error("invalid number of arguments ", length(args))
    havemod = length(args) == 3
    local mod
    if havemod
        mod, frame, node = args
    else
        frame, node = args
    end
    nodetmp = gensym(:node)  # used to hoist, e.g., args[4]
    if havemod
        fallback = quote
            isa($nodetmp, Symbol) ? getfield($(esc(mod)), $nodetmp) :
            $nodetmp
        end
    else
        fallback = quote
            $nodetmp
        end
    end
    quote
        $nodetmp = $(esc(node))
        isa($nodetmp, SSAValue) ? lookup_var($(esc(frame)), $nodetmp) :
        isa($nodetmp, GlobalRef) ? lookup_var($(esc(frame)), $nodetmp) :
        isa($nodetmp, SlotNumber) ? lookup_var($(esc(frame)), $nodetmp) :
        isa($nodetmp, QuoteNode) ? $nodetmp.value :
        isa($nodetmp, Symbol) ? getfield(moduleof($(esc(frame))), $nodetmp) :
        isa($nodetmp, Expr) ? lookup_expr($(esc(frame)), $nodetmp) :
        $fallback
    end
end

# This is used only for new struct/abstract/primitive nodes.
# The most important issue is that in these expressions, :call Exprs can be nested,
# and hence our re-use of the `callargs` field of JuliaStackFrame would introduce
# bugs. Since these nodes use a very limited repertoire of calls, we can special-case
# this quite easily.
function lookup_or_eval(stack, frame, @nospecialize(node), pc)
    if isa(node, SSAValue)
        return lookup_var(frame, node)
    elseif isa(node, SlotNumber)
        return lookup_var(frame, node)
    elseif isa(node, Symbol)
        return getfield(moduleof(frame), node)
    elseif isa(node, Int)
        return node
    elseif isa(node, QuoteNode)
        return node.value
    elseif isa(node, Expr)
        ex = Expr(node.head)
        for arg in node.args
            push!(ex.args, lookup_or_eval(stack, frame, arg, pc))
        end
        if ex.head == :call
            f = ex.args[1]
            if f === Core.svec
                return Core.svec(ex.args[2:end]...)
            elseif f === Core.apply_type
                return Core.apply_type(ex.args[2:end]...)
            elseif f === Core.typeof
                return Core.typeof(ex.args[2])
            elseif f === Base.getproperty
                return Base.getproperty(ex.args[2], ex.args[3])
            else
                error("unknown call f ", f)
            end
        else
            dump(ex)
            error("unknown expr ", ex)
        end
    elseif isa(node, Type)
        return node
    end
    return eval_rhs(stack, frame, node, pc)
end

instantiate_type_in_env(arg, spsig, spvals) =
    ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), arg, spsig, spvals)

function resolvefc(frame, @nospecialize expr)
    if isa(expr, SlotNumber)
        expr = lookup_var(frame, expr)
    end
    (isa(expr, Symbol) || isa(expr, String) || isa(expr, Ptr) || isa(expr, QuoteNode)) && return expr
    isa(expr, Tuple{Symbol,Symbol}) && return expr
    isa(expr, Tuple{String,String}) && return expr
    isa(expr, Tuple{Symbol,String}) && return expr
    isa(expr, Tuple{String,Symbol}) && return expr
    if isexpr(expr, :call)
        a = expr.args[1]
        (isa(a, QuoteNode) && a.value == Core.tuple) || error("unexpected ccall to ", expr)
        return Expr(:call, GlobalRef(Core, :tuple), expr.args[2:end]...)
    end
    error("unexpected ccall to ", expr)
end

function collect_args(frame, call_expr; isfc=false)
    args = frame.callargs
    resize!(args, length(call_expr.args))
    mod = moduleof(frame)
    args[1] = isfc ? resolvefc(frame, call_expr.args[1]) : @lookup(mod, frame, call_expr.args[1])
    for i = 2:length(args)
        args[i] = @lookup(mod, frame, call_expr.args[i])
    end
    return args
end

"""
    ret = evaluate_foreigncall!(stack, frame::JuliaStackFrame, call_expr, pc)

Evaluate a `:foreigncall` (from a `ccall`) statement `callexpr` in the context of `frame`.
`stack` and `pc` are unused, but supplied for consistency with [`evaluate_call!`](@ref).
"""
function evaluate_foreigncall!(stack, frame::JuliaStackFrame, call_expr::Expr, pc)
    args = collect_args(frame, call_expr; isfc=true)
    for i = 2:length(args)
        arg = args[i]
        args[i] = isa(arg, Symbol) ? QuoteNode(arg) : arg
    end
    scope = frame.code.scope
    if !isempty(frame.sparams) && scope isa Method
        sig = scope.sig
        args[2] = instantiate_type_in_env(args[2], sig, frame.sparams)
        args[3] = Core.svec(map(args[3]) do arg
            instantiate_type_in_env(arg, sig, frame.sparams)
        end...)
    end
    return Core.eval(moduleof(frame), Expr(:foreigncall, args...))
end

function evaluate_call!(::Compiled, frame::JuliaStackFrame, call_expr::Expr, pc;  #=unused=# exec!::Function=finish_and_return!)
    ret = maybe_evaluate_builtin(frame, call_expr)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    f = fargs[1]
    if isa(f, CodeInfo)
        error("CodeInfo")
        ret = finish_and_return!(Compiled(), enter_call_expr(frame, call_expr))
    else
        popfirst!(fargs)  # now it's really just `args`
        ret = f(fargs...)
    end
    return ret
end

function evaluate_call!(stack, frame::JuliaStackFrame, call_expr::Expr, pc; exec!::Function=finish_and_return!)
    ret = maybe_evaluate_builtin(frame, call_expr)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    if (f = fargs[1]) === Core.eval
        return Core.eval(fargs[2], fargs[3])  # not a builtin, but worth treating specially
    elseif fargs[1] === Base.rethrow
        err = length(fargs) > 1 ? fargs[2] : frame.last_exception[]
        throw(err)
    end
    framecode, lenv = get_call_framecode(fargs, frame.code, pc.next_stmt)
    if lenv === nothing
        if isa(framecode, Compiled)
            popfirst!(fargs)  # now it's really just `args`
            return f(fargs...)
        end
        return framecode  # this was a Builtin
    end
    frame.pc[] = pc  # to mark position in the frame (e.g., if we hit breakpoint or error)
    push!(stack, frame)
    newframe = build_frame(framecode, fargs, lenv)
    ret = exec!(stack, newframe)
    pop!(stack)
    push!(junk, newframe)  # rather than going through GC, just re-use it
    return ret
end

"""
    ret = evaluate_call!(Compiled(), frame::JuliaStackFrame, call_expr, pc)
    ret = evaluate_call!(stack,      frame::JuliaStackFrame, call_expr, pc)

Evaluate a `:call` expression `call_expr` in the context of `frame`.
The first causes it to be executed using Julia's normal dispatch (compiled code),
whereas the second recurses in via the interpreter. `stack` should be a vector of [`JuliaStackFrame`](@ref).
"""
evaluate_call!

# The following come up only when evaluating toplevel code
function evaluate_methoddef!(stack, frame, node, pc)
    f = node.args[1]
    if isa(f, Symbol)
        mod = moduleof(frame)
        f = isdefined(mod, f) ? getfield(mod, f) : Core.eval(moduleof(frame), Expr(:function, f))  # create a new function
    end
    length(node.args) == 1 && return f
    sig = @lookup(frame, node.args[2])::SimpleVector
    body = @lookup(frame, node.args[3])
    ccall(:jl_method_def, Cvoid, (Any, Any, Any), sig, body, moduleof(frame))
    return nothing
end

function structname(frame, node)
    name = node.args[1]
    if isa(name, GlobalRef)
        mod, name = name.module, name.name
    else
        mod = moduleof(frame)
        name = name::Symbol
    end
    return name, mod
end

function set_structtype_const(mod::Module, name::Symbol)
    dt = Base.unwrap_unionall(getfield(mod, name))
    ccall(:jl_set_const, Cvoid, (Any, Any, Any), mod, dt.name.name, dt.name.wrapper)
end


function evaluate_structtype!(stack, frame, node, pc)
    name, mod = structname(frame, node)
    params = lookup_or_eval(stack, frame, node.args[2], pc)::SimpleVector
    fieldnames = lookup_or_eval(stack, frame, node.args[3], pc)::SimpleVector
    supertype = lookup_or_eval(stack, frame, node.args[4], pc)::Type
    fieldtypes = lookup_or_eval(stack, frame, node.args[5], pc)::SimpleVector
    ismutable = node.args[6]
    ninit = node.args[7]
    Core.eval(mod, Expr(:struct_type, name, params, fieldnames, supertype, fieldtypes, ismutable, ninit))
    VERSION < v"1.2.0-DEV.239" && set_structtype_const(mod, name)
end

function evaluate_abstracttype!(stack, frame, node, pc)
    name, mod = structname(frame, node)
    params = lookup_or_eval(stack, frame, node.args[2], pc)::SimpleVector
    supertype = lookup_or_eval(stack, frame, node.args[3], pc)::Type
    Core.eval(mod, Expr(:abstract_type, name, params, supertype))
    VERSION < v"1.2.0-DEV.239" && set_structtype_const(mod, name)
end

function evaluate_primitivetype!(stack, frame, node, pc)
    name, mod = structname(frame, node)
    params = lookup_or_eval(stack, frame, node.args[2], pc)::SimpleVector
    nbits = node.args[3]::Int
    supertype = lookup_or_eval(stack, frame, node.args[4], pc)::Type
    Core.eval(mod, Expr(:primitive_type, name, params, nbits, supertype))
    VERSION < v"1.2.0-DEV.239" && set_structtype_const(mod, name)
end

function do_assignment!(frame, @nospecialize(lhs), @nospecialize(rhs))
    if isa(lhs, SSAValue)
        frame.ssavalues[lhs.id] = rhs
    elseif isa(lhs, SlotNumber)
        frame.locals[lhs.id] = Some{Any}(rhs)
        frame.last_reference[frame.code.code.slotnames[lhs.id]] =
            lhs.id
    elseif isa(lhs, GlobalRef)
        Core.eval(lhs.mod, :($(lhs.name) = $(QuoteNode(rhs))))
    elseif isa(lhs, Symbol)
        Core.eval(moduleof(frame), :($lhs = $(QuoteNode(rhs))))
    end
end

function eval_rhs(stack, frame, node::Expr, pc)
    head = node.head
    if head == :new
        mod = moduleof(frame)
        rhs = ccall(:jl_new_struct_uninit, Any, (Any,), @lookup(mod, frame, node.args[1]))
        for i = 1:length(node.args) - 1
            ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), rhs, i-1, @lookup(mod, frame, node.args[i+1]))
        end
        return rhs
    elseif head == :splatnew  # Julia 1.2+
        mod = moduleof(frame)
        rhs = ccall(:jl_new_structt, Any, (Any, Any), @lookup(mod, frame, node.args[1]), @lookup(mod, frame, node.args[2]))
        return rhs
    elseif head == :isdefined
        return check_isdefined(frame, node.args[1])
    elseif head == :call
        return evaluate_call!(stack, frame, node, pc)
    elseif head == :foreigncall
        return evaluate_foreigncall!(stack, frame, node, pc)
    elseif head == :copyast
        val = (node.args[1]::QuoteNode).value
        return isa(val, Expr) ? copy(val) : val
    elseif head == :enter
        return length(frame.exception_frames)
    elseif head == :boundscheck
        return true
    elseif head == :meta || head == :inbounds || head == :simdloop || head == :gc_preserve_begin || head == :gc_preserve_end
        return nothing
    elseif head == :method && length(node.args) == 1
        return evaluate_methoddef!(stack, frame, node, pc)
    end
    return lookup_expr(frame, node)
end

function check_isdefined(frame, node)
    if isa(node, SlotNumber)
        return frame.locals[node.id] !== nothing
    elseif isexpr(node, :static_parameter)
        return isassigned(frame.sparams, node.args[1]::Int)
    elseif isa(node, GlobalRef)
        return isdefined(node.mod, node.name)
    elseif isa(node, Symbol)
        return isdefined(moduleof(frame), node)
    end
    error("unrecognized isdefined node ", node)
end


function _step_expr!(stack, frame, @nospecialize(node), pc::JuliaProgramCounter, istoplevel::Bool)
    local rhs
    # show_stackloc(stack, frame, pc)
    # @show node
    try
        if isa(node, Expr)
            if node.head == :(=)
                lhs = node.args[1]
                rhs = node.args[2]
                if isa(rhs, Expr)
                    rhs = eval_rhs(stack, frame, rhs, pc)
                else
                    rhs = istoplevel ? @lookup(moduleof(frame), frame, rhs) : @lookup(frame, rhs)
                end
                do_assignment!(frame, lhs, rhs)
            elseif node.head == :gotoifnot
                arg = @lookup(frame, node.args[1])
                if !isa(arg, Bool)
                    throw(TypeError(nameof(frame), "if", Bool, node.args[1]))
                end
                if !arg
                    return JuliaProgramCounter(node.args[2])
                end
            elseif node.head == :enter
                rhs = node.args[1]
                push!(frame.exception_frames, rhs)
            elseif node.head == :leave
                for _ = 1:node.args[1]
                    pop!(frame.exception_frames)
                end
            elseif node.head == :pop_exception
                n = lookup_var(frame, node.args[1])
                deleteat!(frame.exception_frames, n+1:length(frame.exception_frames))
            elseif node.head == :return
                return nothing
            elseif istoplevel
                if node.head == :method && length(node.args) > 1
                    evaluate_methoddef!(stack, frame, node, pc)
                elseif node.head == :struct_type
                    evaluate_structtype!(stack, frame, node, pc)
                elseif node.head == :abstract_type
                    evaluate_abstracttype!(stack, frame, node, pc)
                elseif node.head == :primitive_type
                    evaluate_primitivetype!(stack, frame, node, pc)
                elseif node.head == :module
                    error("this should have been handled by prepare_toplevel")
                elseif node.head == :using || node.head == :import || node.head == :export
                    Core.eval(moduleof(frame), node)
                elseif node.head == :const
                    g = node.args[1]
                    if isa(g, GlobalRef)
                        mod, name = g.module, g.name
                    else
                        mod, name = moduleof(frame), g::Symbol
                    end
                    if VERSION >= v"1.2.0-DEV.239"  # depends on https://github.com/JuliaLang/julia/pull/30893
                        Core.eval(mod, Expr(:const, name))
                    end
                elseif node.head == :thunk
                    newframe = prepare_thunk(moduleof(frame), node)
                    frame.pc[] = pc
                    if isa(stack, Compiled)
                        finish!(stack, newframe, true)
                    else
                        push!(stack, frame)
                        finish!(stack, newframe, true)
                        pop!(stack)
                        push!(junk, newframe)  # rather than going through GC, just re-use it
                    end
                elseif node.head == :global
                    # error("fixme")
                elseif node.head == :toplevel
                    mod = moduleof(frame)
                    newstack = similar(stack, 0)
                    modexs, _ = prepare_toplevel(mod, node)
                    Core.eval(mod, Expr(:toplevel,
                        :(for modex in $modexs
                              newframe = ($prepare_thunk)(modex)
                              while true
                                  ($through_methoddef_or_done!)($newstack, newframe) === nothing && break
                              end
                          end)))
                elseif node.head == :error
                    error("unexpected error statement ", node)
                elseif node.head == :incomplete
                    error("incomplete statement ", node)
                else
                    rhs = eval_rhs(stack, frame, node, pc)
                end
            elseif node.head == :thunk || node.head == :toplevel
                error("this frame needs to be run at top level")
            else
                rhs = eval_rhs(stack, frame, node, pc)
            end
        elseif isa(node, GotoNode)
            return JuliaProgramCounter(node.label)
        elseif isa(node, NewvarNode)
            # FIXME: undefine the slot?
        elseif istoplevel && isa(node, LineNumberNode)
        elseif istoplevel && isa(node, Symbol)
            rhs = getfield(moduleof(frame), node)
        else
            rhs = @lookup(frame, node)
        end
    catch err
        return handle_err(frame, err)
    end
    if isassign(frame, pc)
        if !@isdefined(rhs)
            @show frame node pc
        end
        lhs = getlhs(pc)
        do_assignment!(frame, lhs, rhs)
    end
    return pc + 1
end

_step_expr!(stack, frame, pc::JuliaProgramCounter, istoplevel::Bool=false) =
    _step_expr!(stack, frame, pc_expr(frame, pc), pc, istoplevel)

"""
    pc = step_expr!(stack, frame)

Execute the next statement in `frame`. `pc` is the new program counter, or `nothing`
if execution terminates.
`stack` controls call evaluation; `stack = Compiled()` evaluates :call expressions
by normal dispatch, whereas a vector of `JuliaStackFrame`s will use recursive interpretation.
"""
function step_expr!(stack, frame, istoplevel::Bool=false)
    pc = _step_expr!(stack, frame, frame.pc[], istoplevel)
    pc === nothing && return nothing
    frame.pc[] = pc
end

function handle_err(frame, err)
    # Check for world age errors, which generally indicate a failure to go back to toplevel
    if isa(err, MethodError)
        is_arg_types = isa(err.args, DataType)
        arg_types = is_arg_types ? err.args : Base.typesof(err.args...)
        if (err.world != typemax(UInt) &&
            hasmethod(err.f, arg_types) &&
            !hasmethod(err.f, arg_types, world = err.world))
            @warn "likely failure to return to toplevel, try JuliaInterpreter.prepare_toplevel"
            rethrow(err)
        end
    end
    isempty(frame.exception_frames) && rethrow(err)
    frame.last_exception[] = err
    return JuliaProgramCounter(frame.exception_frames[end])
end

"""
    pc = finish!(stack, frame, pc=frame.pc[])

Run `frame` until execution terminates. `pc` is the program counter for the final statement.
`stack` controls call evaluation; `stack = Compiled()` evaluates :call expressions
by normal dispatch, whereas a vector of `JuliaStackFrame`s will use recursive interpretation.
"""
function finish!(stack, frame, pc::JuliaProgramCounter=frame.pc[], istoplevel::Bool=false)
    while true
        new_pc = _step_expr!(stack, frame, pc, istoplevel)
        new_pc == nothing && break
        pc = new_pc
    end
    frame.pc[] = pc
end
finish!(stack, frame, istoplevel::Bool) = finish!(stack, frame, frame.pc[], istoplevel)

"""
    ret = finish_and_return!(stack, frame, istoplevel::Bool=false)
    ret = finish_and_return!(stack, frame, pc, istoplevel::Bool)

Run `frame` until execution terminates, and pass back the computed return value.
`stack` controls call evaluation; `stack = Compiled()` evaluates :call expressions
by normal dispatch, whereas a vector of `JuliaStackFrame`s will use recursive interpretation.

Optionally supply the starting `pc`, if you don't want to start at the current location in `frame`.
"""
function finish_and_return!(stack, frame, pc::JuliaProgramCounter=frame.pc[], istoplevel::Bool=false)
    pc = finish!(stack, frame, pc, istoplevel)
    return get_return(frame, pc)
end
finish_and_return!(stack, frame, istoplevel::Bool) = finish_and_return!(stack, frame, frame.pc[], istoplevel)

"""
    ret = get_return(frame, pc=frame.pc[])

Get the return value of `frame`. Throws an error if `pc` does not point to a `return` expression.
`frame` must have already been executed so that the return value has been computed (see,
e.g., [`JuliaInterpreter.finish!`](@ref)).
"""
function get_return(frame, pc = frame.pc[])
    node = pc_expr(frame, pc)
    isexpr(node, :return) || error("expected return statement, got ", node)
    return @lookup(frame, (node::Expr).args[1])
end
get_return(frame::Tuple{Module,Expr,JuliaStackFrame}) = get_return(frame[end])

function is_call(node)
    isexpr(node, :call) ||
    (isexpr(node, :(=)) && (isexpr(node.args[2], :call)))
end

"""
    next_until!(predicate, stack, frame, pc=frame.pc[])

Step through statements of `frame` until the next statement satifies `predicate(stmt)`.
"""
function next_until!(f, stack, frame, pc::JuliaProgramCounter=frame.pc[], istoplevel::Bool=false)
    while (newpc = _step_expr!(stack, frame, pc, istoplevel)) != nothing
        pc = newpc
        f(pc_expr(frame, pc)) && (frame.pc[] = pc; return pc)
    end
    frame.pc[] = pc
    return nothing
end
next_until!(f, stack, frame, istoplevel::Bool) = next_until!(f, stack, frame, frame.pc[], istoplevel)
next_call!(stack, frame, pc=frame.pc[]) = next_until!(node->is_call(node)||isexpr(node,:return), stack, frame, pc)

"""
    through_methoddef_or_done!(stack, frame)

Runs `frame` at top level until it either finishes (e.g., hits a `return` statement)
or defines a new method.
"""
function through_methoddef_or_done!(stack, frame::JuliaStackFrame)
    predicate(stmt) = isexpr(stmt, :method, 3) || isexpr(stmt, :thunk)
    pc = next_until!(predicate, stack, frame, true)
    pc === nothing && return nothing
    return _step_expr!(stack, frame, pc, true)
end
through_methoddef_or_done!(stack, frame::Tuple{Module,Expr,JuliaStackFrame}) =
    through_methoddef_or_done!(stack, frame[end])
through_methoddef_or_done!(stack, modex::Tuple{Module,Expr,Expr}) = Core.eval(modex[1], modex[3])

function changed_line!(expr, line, fls)
    if length(fls) == 1 && isa(expr, LineNumberNode)
        return expr.line != line
    elseif length(fls) == 1 && isa(expr, Expr) && isexpr(expr, :line)
        return expr.args[1] != line
    else
        if is_loc_meta(expr, :pop_loc)
            pop!(fls)
        elseif is_loc_meta(expr, :push_loc)
            push!(fls,(expr.args[2],0))
        end
        return false
    end
end

"""
Determine whether we are calling a function for which the current function
is a wrapper (either because of optional arguments or becaue of keyword arguments).
"""
function iswrappercall(expr)
    isexpr(expr, :(=)) && (expr = expr.args[2])
    isexpr(expr, :call) && any(x->x==SlotNumber(1), expr.args)
end

pc_expr(frame, pc) = frame.code.code.code[pc.next_stmt]
pc_expr(frame) = pc_expr(frame, frame.pc[])

function find_used(code::CodeInfo)
    used = BitSet()
    stmts = code.code
    for stmt in stmts
        Core.Compiler.scan_ssa_use!(push!, used, stmt)
        if isexpr(stmt, :struct_type)  # this one is missed
            for a in stmt.args
                Core.Compiler.scan_ssa_use!(push!, used, a)
            end
        end
    end
    return used
end

isgotonode(node) = isa(node, GotoNode) || isexpr(node, :gotoifnot)

linenumber(frame) = linenumber(frame, frame.pc[])
function linenumber(frame, pc)
    codeloc = frame.code.code.codelocs[pc.next_stmt]
    return frame.code.scope isa Method ?
        frame.code.code.linetable[codeloc].line :
        codeloc
end
function next_line!(stack, frame, dbstack = nothing)
    initial = linenumber(frame)
    first = true
    pc = frame.pc[]
    while linenumber(frame, pc) == initial
        # If this is a return node, interrupt execution. This is the same
        # special case as in `s`.
        expr = pc_expr(frame, pc)
        (!first && isexpr(expr, :return)) && return pc
        first = false
        # If this is a goto node, step it and reevaluate
        if isgotonode(expr)
            pc = _step_expr!(stack, frame, pc)
            pc == nothing && return nothing
        elseif dbstack !== nothing && iswrappercall(expr)
            # With splatting it can happen that we do something like ssa = tuple(#self#), _apply(ssa), which
            # confuses the logic here, just step into the first call that's not a builtin
            while true
                dbstack[1] = JuliaStackFrame(JuliaFrameCode(frame.code; wrapper = true), frame, pc)
                call_expr = pc_expr(frame, pc)
                isexpr(call_expr, :(=)) && (call_expr = call_expr.args[2])
                call_expr = Expr(:call, map(x->@lookup(frame, x), call_expr.args)...)
                new_frame = enter_call_expr(call_expr)
                if new_frame !== nothing
                    pushfirst!(dbstack, new_frame)
                    frame = new_frame
                    pc = frame.pc[]
                    break
                else
                    pc = _step_expr!(stack, frame, pc)
                    pc == nothing && return nothing
                end
            end
        else
            pc = _step_expr!(stack, frame, pc)
            pc == nothing && return nothing
        end
        frame.pc[] = pc
    end
    maybe_next_call!(stack, frame, pc)
end

function maybe_next_call!(stack, frame, pc)
    call_or_return(node) = is_call(node) || isexpr(node, :return)
    call_or_return(pc_expr(frame, pc)) ||
        (pc = next_until!(call_or_return, stack, frame, pc, false))
    pc
end
maybe_next_call!(stack, frame) = maybe_next_call!(stack, frame, frame.pc[])
