# Implements a simple interpreter for julia's lowered AST

lookup_var(frame, val::SSAValue) = frame.ssavalues[val.id+1]
lookup_var(frame, ref::GlobalRef) = getfield(ref.mod, ref.name)
lookup_var(frame, slot::SlotNumber) = get(frame.locals[slot.id])
function lookup_var(frame, e::Expr)
    isexpr(e, :static_parameter) || error()
    frame.sparams[e.args[1]]
end

function evaluate_call(frame, call_expr)
    # Don't go through eval since this may have unqouted, symbols and
    # exprs
    f = to_function(lookup_var(frame, call_expr.args[1]))
    args = Array{Any}(length(call_expr.args)-1)
    for i = 1:length(args)
        arg = call_expr.args[i+1]
        args[i] = isa(arg, Union{SSAValue, GlobalRef, Slot}) ? lookup_var(frame, arg) :
            arg
    end
    if isa(f, Core.IntrinsicFunction)
        # Special handling to quote any literal symbols that may still
        # be in here, so we can pass it into eval
        args = map(args) do arg
            isa(arg, Union{Symbol, GlobalRef}) ? QuoteNode(node.args[i]) : arg
        end
        ret = eval(node)
    elseif isa(f, CodeInfo)
        ret = finish!(enter_call_expr(frame, call_expr))
    else
        # Don't go through eval since this may have unqouted, symbols and
        # exprs
        ret = f(args...)
    end
    return ret
end

function _step_expr(frame, pc)
    node = pc_expr(frame, pc)
    local ret
    try
        if isa(node, Expr)
            if node.head == :(=)
                lhs = node.args[1]
                rhs = isexpr(node.args[2], :call) ? evaluate_call(frame, node.args[2]) :
                    lookup_var(frame, node.args[2])
                if isa(lhs, SSAValue)
                    frame.ssavalues[lhs.id+1] = rhs
                elseif isa(lhs, Slot)
                    frame.locals[lhs.id] = Nullable{Any}(rhs)
                    frame.last_reference[frame.code.slotnames[lhs.id]] =
                        lhs.id
                elseif isa(lhs, GlobalRef)
                    eval(lhs.mod,:($(lhs.name) = $(QuoteNode(rhs))))
                end
                # Special case hack for readability.
                # ret = rhs
                ret = node
            elseif node.head == :&
                ret = node
            elseif node.head == :gotoifnot
                ret = node
                arg = lookup_var(frame, node.args[1])
                if !isa(arg, Bool)
                    throw(TypeError(frame.meth.name, "if", Bool, node.args[1]))
                end
                if !arg
                    return JuliaProgramCounter(node.args[2])
                end
            elseif node.head == :call
                evaluate_call(frame, node)
            elseif node.head == :static_typeof
                ret = Any
            elseif node.head == :type_goto
                ret = nothing
            elseif node.head == :enter
                push!(interp.exception_frames, node.args[1])
                ret = node
            elseif node.head == :leave
                for _ = 1:node.args[1]
                    pop!(interp.exception_frames)
                end
                ret = node
            elseif node.head == :static_parameter
                ret = interp.env.sparams[node.args[1]]
            elseif node.head == :return
                return nothing
            else
                ret = eval(node)
            end
        elseif isa(node, GotoNode)
            return JuliaProgramCounter(node.args[1])
        elseif isa(node, QuoteNode)
            ret = node.value
        else
            ret = eval(node)
        end
    catch err
        rethrow(err)
    end
    return JuliaProgramCounter(pc.next_stmt + 1)
end
step_expr(interp) = (r = _step_expr(interp); done!(interp); r)

function next_statement!(interp)
    ind, node = interp.next_expr
    move_past = ind[1]
    while step_expr(interp)
        ind, node = interp.next_expr
        if ind[1] != move_past
            return true
        end
    end
    return false
end

function next_until!(f,interp)
    ind, node = interp.next_expr
    while step_expr(interp)
        ind, node = interp.next_expr
        f(node) && return true
    end
    return false
end
next_call!(interp) = next_until!(node->isexpr(node,:call)||isexpr(node,:return), interp)

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
function iswrappercall(interp, expr)
    !isexpr(expr, :call) && return false
    r = determine_method_for_expr(interp, expr; enter_generated = false)
    if r !== nothing
        linfo, method, args, _ = r
        ours, theirs = interp.linfo.def, method
        # Check if this a method of the same function that shares a definition line/file.
        # If so, we're likely in an automatically generated wrapper.
        if ours.sig.parameters[1] == theirs.sig.parameters[1] &&
            ours.line == theirs.line && ours.file == theirs.file
            return true
        end
    end
    return false
end

pc_expr(frame, pc) = frame.code.code[pc.next_stmt]

function next_line!(frame; state = nothing)
    didchangeline = false
    fls = determine_line_and_file(frame, frame.pc.next_stmt)
    line = fls[1][2]
    first = true
    pc = frame.pc
    while !didchangeline
        # If this is a return node, interrupt execution. This is the same
        # special case as in `s`.
        (!first && isexpr(pc_expr(frame, pc), :return)) && return pc
        first = false
        # If this is a goto node, step it and reevaluate
        if isgotonode(pc_expr(frame, pc))
            pc = _step_expr(frame, pc)
            pc == nothing && return nothing
            fls = determine_line_and_file(frame, pc.next_stmt)
            didchangeline = line != fls[1][2]
        elseif iswrappercall(frame, pc_expr(frame, pc))
            interp.did_wrappercall = true
            frame = enter_call_expr(frame, pc_expr(frame, pc))
        elseif isa(pc_expr(frame, pc), LineNumberNode)
            line != pc_expr(frame, pc).line && break
            pc = _step_expr(frame, pc)
        else
            pc = _step_expr(frame, pc)
            pc == nothing && return nothing
        end
    end
    # Ok, we stepped to the next line. Now step through to the next call
    # call_or_assignment(node) = isexpr(node,:call) || isexpr(node,:(=)) || isexpr(node, :return)
    # call_or_assignment(interp.next_expr[2]) ||
    #    next_until!(call_or_assignment, interp)
    pc
end

function advance_to_line(interp, line)
    while true
        at_line = determine_line_and_file(interp, idx_stack(interp))[1][2]
        at_line == line && break
        next_line!(interp) || break
    end
end

function _evaluated!(interp, ret, wasstaged = false)
    if wasstaged
        # If this is the result of a staged function, we replace the argument
        # the call rather than the call itself
        ind, node = interp.next_expr
        @assert isexpr(node, :call)
        interp.shadowtree[[ind.idx_stack.stack; 1]] = (ret, AnnotationNode{Any}(true,AnnotationNode{Any}[]))
    else
        ind, node = interp.next_expr
        interp.shadowtree[ind.idx_stack.stack] = (ret, AnnotationNode{Any}(true,AnnotationNode{Any}[]))
    end
end
evaluated!(interp, ret, wasstaged = false) = (_evaluated!(interp, ret, wasstaged); done!(interp))

"""
Advance to the next evaluatable statement
"""
function done!(interp)
    ind, node = interp.next_expr
    # Skip evaluated values (e.g. constants)
    while interp.shadowtree.shadow[ind.idx_stack.stack].val
        ind, node = next_expr!(interp)
    end
    return true
end