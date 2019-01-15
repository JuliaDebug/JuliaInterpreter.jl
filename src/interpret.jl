# Implements a simple interpreter for julia's lowered AST

getlhs(pc) = SSAValue(pc.next_stmt)

isassign(fr) = isassign(fr, fr.pc[])
isassign(fr, pc) = (pc.next_stmt in fr.code.used)

lookup_var(frame, val::SSAValue) = frame.ssavalues[val.id+1]
lookup_var(frame, ref::GlobalRef) = getfield(ref.mod, ref.name)
function lookup_var(frame, slot::SlotNumber)
    val = frame.locals[slot.id]
    val !== nothing && return val.value
    error("slot not assigned")
end

function lookup_expr(frame, e::Expr, fallback::Bool)
    head = e.head
    head == :the_exception && return frame.last_exception[]
    head == :boundscheck && return true
    head == :static_parameter && return frame.sparams[e.args[1]::Int]
    return fallback ? e : error()
end

"""
    rhs = @eval_rhs(f, frame, node)

This macro substitutes for a function call, as a performance optimization to avoid dynamic dispatch.
It calls `lookup_var(frame, node)` when appropriate, otherwise:

* with arbitrary `f` it will evaluate arbitrary expressions (handling calls with `f`; e.g., `f=evaluate_call_compiled`)
* with `f=true` it will try `lookup_expr`
* with `f=false` it throws an error.
"""
macro eval_rhs(feval, frame, node)
    if feval == true
        fallback = quote
            isa($(esc(node)), Expr) ? lookup_expr($(esc(frame)), $(esc(node)), true) : $(esc(node))
        end
    elseif feval == false
        fallback = :(error("bad node ", node))
    else
        fallback = quote
            isa($(esc(node)), QuoteNode) ? $(esc(node)).value :
            isa($(esc(node)), Expr) ? eval_rhs($(esc(feval)), $(esc(frame)), $(esc(node))) :
            $(esc(node))
        end
    end
    quote
        isa($(esc(node)), SSAValue) ? lookup_var($(esc(frame)), $(esc(node))) :
        isa($(esc(node)), GlobalRef) ? lookup_var($(esc(frame)), $(esc(node))) :
        isa($(esc(node)), SlotNumber) ? lookup_var($(esc(frame)), $(esc(node))) :
        $fallback
    end
end

instantiate_type_in_env(arg, spsig, spvals) =
    ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), arg, spsig, spvals)

function maybe_evaluate_nested(feval, frame, call_expr)
    # Core.svec and Core.apply_type are the only two common "nested" calls;
    # by special-casing them we get performance advantages.
    nestedargs(args) = Any[@eval_rhs(feval, frame, a) for a in Iterators.drop(args, 1)]

    if isa(call_expr.args[1], GlobalRef)
        g = call_expr.args[1]::GlobalRef
        if g.mod == Core && g.name == :svec
            return Core.svec(nestedargs(call_expr.args)...)
        elseif g.mod == Core && g.name == :apply_type
            return Core.apply_type(nestedargs(call_expr.args)...)
        end
    end
    return call_expr
end

function collect_args(feval, frame, call_expr)
    args = Vector{Any}(undef, length(call_expr.args))
    for i = 1:length(args)
        args[i] = @eval_rhs(feval, frame, call_expr.args[i])
    end
    return args
end

function evaluate_foreigncall(feval, frame, call_expr)
    args = collect_args(feval, frame, call_expr)
    for i = 1:length(args)
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

function evaluate_call_compiled(frame, call_expr)
    ret = maybe_evaluate_nested(evaluate_call_compiled, frame, call_expr)
    isexpr(ret, :call) || return ret
    args = collect_args(evaluate_call_compiled, frame, call_expr)
    isa(args, Vector{Any}) || return args
    # Don't go through eval since this may have unquoted symbols and
    # exprs
    f = to_function(args[1])
    if isa(f, CodeInfo)
        ret = finish_and_return!(evaluate_call_compiled, enter_call_expr(frame, call_expr))
    else
        popfirst!(args)
        ret = f(args...)
    end
    return ret
end

function evaluate_call_interpreted!(stack, frame, call_expr)
    feval(frm, nd) = evaluate_call_interpreted!(stack, frm, nd)

    ret = maybe_evaluate_nested(feval, frame, call_expr)
    isexpr(ret, :call) || return ret
    args = collect_args(feval, frame, call_expr)
    isa(args, Vector{Any}) || return args
    f = to_function(args[1])
    popfirst!(args)
    if isa(f, CodeInfo)
        error("FIXME")
    end
    if f isa Core.Builtin || f isa Core.IntrinsicFunction  # TODO: make this faster
        ret = f(args...)
    else
        frame = enter_call(f, args...)
        push!(stack, frame)
        ret = finish_and_return!(feval, frame)
        pop!(stack)
    end
    return ret
end

function do_assignment!(frame, @nospecialize(lhs), @nospecialize(rhs))
    if isa(lhs, SSAValue)
        frame.ssavalues[lhs.id+1] = rhs
    elseif isa(lhs, SlotNumber)
        frame.locals[lhs.id] = Some{Any}(rhs)
        frame.last_reference[frame.code.code.slotnames[lhs.id]] =
            lhs.id
    elseif isa(lhs, GlobalRef)
        Base.eval(lhs.mod,:($(lhs.name) = $(QuoteNode(rhs))))
    end
end

function eval_rhs(feval, frame, node::Expr)
    head = node.head
    if head == :new
        new_expr = Expr(:new, map(x->QuoteNode(@eval_rhs(true, frame, x)),
            node.args)...)
        rhs = Core.eval(moduleof(frame), new_expr)
    elseif head == :isdefined
        rhs = check_isdefined(frame, node.args[1])
    elseif head == :enter
        rhs = length(frame.exception_frames)
    elseif head == :call
        rhs = feval(frame, node)
    elseif head == :foreigncall
        rhs = evaluate_foreigncall(feval, frame, node)
    else
        rhs = lookup_expr(frame, node, false)
    end
    if isa(rhs, QuoteNode)
        rhs = rhs.value
    end
    return rhs
end

check_isdefined(frame, node::Slot) = isassigned(frame.locals, slot.id)
function check_isdefined(frame, node::Expr)
    node.head == :static_parameter && return isassigned(frame.sparams, node.args[1])
end

function _step_expr(feval, frame, pc)
    node = pc_expr(frame, pc)
    local rhs
    try
        if isa(node, Expr)
            if node.head == :(=)
                lhs = node.args[1]
                rhs = @eval_rhs(feval, frame, node.args[2])
                do_assignment!(frame, lhs, rhs)
                # Special case hack for readability.
                # ret = rhs
            elseif node.head == :gotoifnot
                arg = @eval_rhs(feval, frame, node.args[1])
                if !isa(arg, Bool)
                    throw(TypeError(nameof(frame), "if", Bool, node.args[1]))
                end
                if !arg
                    return JuliaProgramCounter(node.args[2])
                end
            elseif node.head == :call
                rhs = feval(frame, node)
            elseif node.head ==  :foreigncall
                rhs = evaluate_foreigncall(feval, frame, node)
            elseif node.head == :new
                rhs = eval_rhs(feval, frame, node)
            elseif node.head == :static_typeof || node.head == :type_goto
                error(node, " still exists")
            elseif node.head == :inbounds
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
            elseif node.head == :isdefined
                rhs = check_isdefined(frame, node.args[1])
            elseif node.head == :static_parameter
                rhs = frame.sparams[node.args[1]]
            elseif node.head == :gc_preserve_end || node.head == :gc_preserve_begin
            elseif node.head == :return
                return nothing
            else
                ret = eval(node)
            end
        elseif isa(node, GotoNode)
            return JuliaProgramCounter(node.label)
        elseif isa(node, QuoteNode)
            rhs = node.value
        else
            rhs = @eval_rhs(feval, frame, node)
        end
    catch err
        isempty(frame.exception_frames) && rethrow(err)
        frame.last_exception[] = err
        return JuliaProgramCounter(frame.exception_frames[end])
    end
    if isassign(frame, pc)
        if !@isdefined(rhs)
            @show node pc
        end
        lhs = getlhs(pc)
        do_assignment!(frame, lhs, rhs)
    end
    return pc + 1
end

function step_expr(feval, frame)
    pc = _step_expr(feval, frame, frame.pc[])
    pc === nothing && return nothing
    frame.pc[] = pc
end

function finish!(feval, frame, pc=frame.pc[])
    while true
        new_pc = _step_expr(feval, frame, pc)
        new_pc == nothing && break
        pc = new_pc
    end
    frame.pc[] = pc
end

function finish_and_return!(feval, frame, pc=frame.pc[])
    pc = finish!(feval, frame, pc)
    node = pc_expr(frame, pc)
    isexpr(node, :return) || error("unexpected node ", node)
    return @eval_rhs(feval, frame, node.args[1])
end

function is_call(node)
    isexpr(node, :call) ||
    (isexpr(node, :(=)) && isexpr(node.args[2], :call))
end

function next_until!(f, feval, frame, pc=frame.pc[])
    while (pc = _step_expr(feval, frame, pc)) != nothing
        f(pc_expr(frame, pc)) && (frame.pc[] = pc; return pc)
    end
    return nothing
end
next_call!(feval, frame, pc=frame.pc[]) = next_until!(node->is_call(node)||isexpr(node,:return), feval, frame, pc)

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

isgotonode(node) = isa(node, GotoNode) || isexpr(node, :gotoifnot)

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
    end
    return used
end

function maybe_next_call!(feval, frame, pc)
    call_or_return(node) = is_call(node) || isexpr(node, :return)
    call_or_return(pc_expr(frame, pc)) ||
        (pc = next_until!(call_or_return, feval, frame, pc))
    pc
end
maybe_next_call!(feval, frame) = maybe_next_call!(feval, frame, frame.pc[])

location(frame) = location(frame, frame.pc[])
function location(frame, pc)
    ln = frame.code.code.codelocs[pc.next_stmt]
    return frame.code.scope isa Method ? ln + frame.code.scope.line - 1 : ln
end
function next_line!(feval, frame, stack = nothing)
    initial = location(frame)
    first = true
    pc = frame.pc[]
    while location(frame, pc) == initial
        # If this is a return node, interrupt execution. This is the same
        # special case as in `s`.
        (!first && isexpr(pc_expr(frame, pc), :return)) && return pc
        first = false
        # If this is a goto node, step it and reevaluate
        if isgotonode(pc_expr(frame, pc))
            pc = _step_expr(feval, frame, pc)
            pc == nothing && return nothing
        elseif stack !== nothing && iswrappercall(pc_expr(frame, pc))
            # With splatting it can happen that we do something like ssa = tuple(#self#), _apply(ssa), which
            # confuses the logic here, just step into the first call that's not a builtin
            while true
                stack[1] = JuliaStackFrame(JuliaFrameCode(frame.code; wrapper = true), frame, pc)
                call_expr = pc_expr(frame, pc)
                isexpr(call_expr, :(=)) && (call_expr = call_expr.args[2])
                call_expr = Expr(:call, map(x->@eval_rhs(true, frame, x), call_expr.args)...)
                new_frame = enter_call_expr(call_expr)
                if new_frame !== nothing
                    pushfirst!(stack, new_frame)
                    frame = new_frame
                    pc = frame.pc[]
                    break
                else
                    pc = _step_expr(feval, frame, pc)
                    pc == nothing && return nothing
                end
            end
        elseif isa(pc_expr(frame, pc), LineNumberNode)
            line != pc_expr(frame, pc).line && break
            pc = _step_expr(feval, frame, pc)
        else
            pc = _step_expr(feval, frame, pc)
            pc == nothing && return nothing
        end
        frame.pc[] = pc
    end
    maybe_next_call!(feval, frame, pc)
end
