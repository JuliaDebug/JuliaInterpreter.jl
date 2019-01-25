# Utilities for going from lowered code back to surface AST
# The goal is to delete this.

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

function astypeparam(t)
    if isa(t, Int) || isa(t, Symbol)
        return t
    else
        return t.name
    end
end

asexpr(name::Symbol, params) = isempty(params) ? name : Expr(:curly, name, map(astypeparam, params)...)

function lookup_or_eval(stack, frame, node, pc)
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
            else
                error("unknown call f ", f)
            end
        else
            dump(ex)
            error("unknown expr ", ex)
        end
    end
    return eval_rhs(stack, frame, node, pc)
end

