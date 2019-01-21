# Implements a simple interpreter for julia's lowered AST

getlhs(pc) = SSAValue(pc.next_stmt)

isassign(fr) = isassign(fr, fr.pc[])
isassign(fr, pc) = (pc.next_stmt in fr.code.used)

lookup_var(frame, val::SSAValue) = frame.ssavalues[val.id+1]
lookup_var(frame, ref::GlobalRef) = getfield(ref.mod, ref.name)
function lookup_var(frame, slot::SlotNumber)
    val = frame.locals[slot.id]
    val !== nothing && return val.value
    error("slot ", slot, " not assigned")
end

function lookup_expr(frame, e::Expr, fallback::Bool)
    head = e.head
    head == :the_exception && return frame.last_exception[]
    head == :boundscheck && return true
    head == :static_parameter && return frame.sparams[e.args[1]::Int]
    return fallback ? e : error()
end

"""
    rhs = @eval_rhs(flag, frame, node)
    rhs = @eval_rhs(flag, frame, node, pc)

This macro substitutes for a function call, as a performance optimization to avoid dynamic dispatch.
It calls `lookup_var(frame, node)` when appropriate, otherwise:

* with `flag=false` it throws an error (if `lookup_var` didn't already handle the call)
* with `flag=true` it will additionally try `lookup_expr`, resolving `QuoteNode`s, and otherwise return `node`
* with `flag=Compiled()` it will additionally recurse into calls using Julia's normal compiled-code evaluation
* with `flag=stack`, a vector of `JuliaStackFrames`, it will recurse via the interpreter

If `flag` isn't a Bool you need to supply `pc`.
"""
macro eval_rhs(flag, frame, rest...)
    node = rest[1]
    pc = length(rest) == 1 ? nothing : rest[2]
    nodetmp = gensym(:node)  # used to hoist, e.g., args[4]
    if flag == true
        fallback = quote
            isa($nodetmp, QuoteNode) ? $nodetmp.value :
            isa($nodetmp, Expr) ? lookup_expr($(esc(frame)), $nodetmp, true) : $nodetmp
        end
    elseif flag == false
        fallback = :(error("bad node ", $nodetmp))
    else
        fallback = quote
            isa($nodetmp, QuoteNode) ? $nodetmp.value :
            isa($nodetmp, Expr) ? eval_rhs($(esc(flag)), $(esc(frame)), $nodetmp, $(esc(pc))) :
            $nodetmp
        end
    end
    quote
        $nodetmp = $(esc(node))
        isa($nodetmp, SSAValue) ? lookup_var($(esc(frame)), $nodetmp) :
        isa($nodetmp, GlobalRef) ? lookup_var($(esc(frame)), $nodetmp) :
        isa($nodetmp, SlotNumber) ? lookup_var($(esc(frame)), $nodetmp) :
        $fallback
    end
end

instantiate_type_in_env(arg, spsig, spvals) =
    ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), arg, spsig, spvals)

function collect_args(frame, call_expr)
    args = frame.callargs
    resize!(args, length(call_expr.args))
    for i = 1:length(args)
        args[i] = @eval_rhs(true, frame, call_expr.args[i])
    end
    return args
end

function evaluate_foreigncall!(stack, frame, call_expr, pc)
    args = collect_args(frame, call_expr)
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

function evaluate_call!(::Compiled, frame, call_expr::Expr, pc)
    ret = maybe_evaluate_builtin(frame, call_expr)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    # Don't go through eval since this may have unquoted symbols and
    # exprs
    f = to_function(fargs[1])
    if isa(f, CodeInfo)
        error("CodeInfo")
        ret = finish_and_return!(Compiled(), enter_call_expr(frame, call_expr))
    else
        popfirst!(fargs)  # now it's really just `args`
        ret = f(fargs...)
    end
    return ret
end

function evaluate_call!(stack, frame, call_expr::Expr, pc)
    ret = maybe_evaluate_builtin(frame, call_expr)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    framecode, lenv = get_call_framecode(fargs, frame.code, pc.next_stmt)
    if lenv === nothing
        return framecode  # this was a Builtin
    end
    frame.pc[] = pc  # to mark position in the frame (e.g., if we hit breakpoint or error)
    push!(stack, frame)
    newframe = build_frame(framecode, fargs, lenv)
    ret = finish_and_return!(stack, newframe)
    pop!(stack)
    push!(junk, newframe)  # rather than going through GC, just re-use it
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

function eval_rhs(stack, frame, node::Expr, pc)
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
        rhs = evaluate_call!(stack, frame, node, pc)
    elseif head == :foreigncall
        rhs = evaluate_foreigncall!(stack, frame, node, pc)
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

function _step_expr!(stack, frame, pc)
    node = pc_expr(frame, pc)
    local rhs
    try
        if isa(node, Expr)
            if node.head == :(=)
                lhs = node.args[1]
                rhs = @eval_rhs(stack, frame, node.args[2], pc)
                do_assignment!(frame, lhs, rhs)
                # Special case hack for readability.
                # ret = rhs
            elseif node.head == :gotoifnot
                arg = @eval_rhs(stack, frame, node.args[1], pc)
                if !isa(arg, Bool)
                    throw(TypeError(nameof(frame), "if", Bool, node.args[1]))
                end
                if !arg
                    return JuliaProgramCounter(node.args[2])
                end
            elseif node.head == :call
                rhs = evaluate_call!(stack, frame, node, pc)
            elseif node.head ==  :foreigncall
                rhs = evaluate_foreigncall!(stack, frame, node, pc)
            elseif node.head == :new
                rhs = eval_rhs(stack, frame, node, pc)
            elseif node.head == :static_typeof || node.head == :type_goto
                error(node, ", despite the docs, still exists")
            elseif node.head == :meta || node.head == :inbounds || node.head == :simdloop
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
                rhs = @eval_rhs(true, frame, node.args[1])
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
            rhs = @eval_rhs(stack, frame, node, pc)
        end
    catch err
        isempty(frame.exception_frames) && rethrow(err)
        frame.last_exception[] = err
        return JuliaProgramCounter(frame.exception_frames[end])
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

function step_expr!(stack, frame)
    pc = _step_expr!(stack, frame, frame.pc[])
    pc === nothing && return nothing
    frame.pc[] = pc
end

function finish!(stack, frame, pc=frame.pc[])
    while true
        new_pc = _step_expr!(stack, frame, pc)
        new_pc == nothing && break
        pc = new_pc
    end
    frame.pc[] = pc
end

function finish_and_return!(stack, frame, pc=frame.pc[])
    pc = finish!(stack, frame, pc)
    node = pc_expr(frame, pc)
    isexpr(node, :return) || error("unexpected node ", node)
    return @eval_rhs(stack, frame, (node::Expr).args[1], pc)
end

function is_call(node)
    isexpr(node, :call) ||
    (isexpr(node, :(=)) && (isexpr(node.args[2], :call)))
end

function next_until!(f, stack, frame, pc=frame.pc[])
    while (pc = _step_expr!(stack, frame, pc)) != nothing
        f(plain(pc_expr(frame, pc))) && (frame.pc[] = pc; return pc)
    end
    return nothing
end
next_call!(stack, frame, pc=frame.pc[]) = next_until!(node->is_call(node)||isexpr(node,:return), stack, frame, pc)

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
        Core.Compiler.scan_ssa_use!(push!, used, plain(stmt))
    end
    return used
end

function maybe_next_call!(stack, frame, pc)
    call_or_return(node) = is_call(node) || isexpr(node, :return)
    call_or_return(plain(pc_expr(frame, pc))) ||
        (pc = next_until!(call_or_return, stack, frame, pc))
    pc
end
maybe_next_call!(stack, frame) = maybe_next_call!(stack, frame, frame.pc[])

location(frame) = location(frame, frame.pc[])
function location(frame, pc)
    ln = frame.code.code.codelocs[pc.next_stmt]
    return frame.code.scope isa Method ? ln + frame.code.scope.line - 1 : ln
end
function next_line!(stack, frame, dbstack = nothing)
    initial = location(frame)
    first = true
    pc = frame.pc[]
    while location(frame, pc) == initial
        # If this is a return node, interrupt execution. This is the same
        # special case as in `s`.
        expr = plain(pc_expr(frame, pc))
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
                call_expr = plain(pc_expr(frame, pc))
                isexpr(call_expr, :(=)) && (call_expr = call_expr.args[2])
                call_expr = Expr(:call, map(x->@eval_rhs(true, frame, x, pc), call_expr.args)...)
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
