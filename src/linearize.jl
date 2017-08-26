# Linearizer. Future versions of julia will emit fully-linear IR, so this way upgrading should be easier.
# This is taken from inference.jl on julia master
function newvar!(code::CodeInfo, typ)
    if isa(code.ssavaluetypes, Int)
        id = code.ssavaluetypes::Int
        code.ssavaluetypes = id + 1
    else
        id = length(code.ssavaluetypes)
        push!(code.ssavaluetypes, typ)
    end
    return SSAValue(id)
end

is_meta_expr_head(head::Symbol) =
(head === :inbounds || head === :boundscheck || head === :meta ||
 head === :line || head === :simdloop)
is_meta_expr(ex::Expr) = is_meta_expr_head(ex.head)

function is_ccall_static(e::Expr)
    if e.head === :call
        length(e.args) == 3 || return false
        for i in 2:3
            a = e.args[i]
            (isa(a, Expr) || isa(a, Slot) || isa(a, SSAValue)) && return false
        end
        return true
    elseif e.head === :static_parameter
        return true
    end
    return false
end

function linearize_arg!(args, i, stmts, code::CodeInfo)
    a = args[i]
    if isa(a, Symbol)
        a = a::Symbol
        typ = Any
    elseif isa(a, GlobalRef)
        a = a::GlobalRef
        typ = Any
    elseif isa(a, Expr)
        typ = (a::Expr).typ
    else
        return
    end
    ssa = newvar!(code, typ)
    push!(stmts, :($ssa = $a))
    args[i] = ssa
    return
end

function get_label_map(body::Vector{Any})
    labelmap = Dict{Int, Int}()
    for i = 1:length(body)
        el = body[i]
        if isa(el, LabelNode)
            labelmap[el.label] = i
        end
    end
    return labelmap
end

function relabel!(body::Vector{Any})
    mapping = get_label_map(body)
    for i = 1:length(body)
        el = body[i]
        if isa(el, LabelNode)
            labelnum = mapping[el.label]
            @assert labelnum !== 0
            body[i] = LabelNode(labelnum)
        elseif isa(el, GotoNode)
            labelnum = mapping[el.label]
            @assert labelnum !== 0
            body[i] = GotoNode(labelnum)
        elseif isa(el, Expr)
            if el.head === :gotoifnot
                labelnum = mapping[el.args[2]::Int]
                if labelnum === 0
                    # Might still have side effects
                    body[i] = el.args[1]
                else
                    el.args[2] = labelnum
                end
            elseif el.head === :enter
                labelnum = mapping[el.args[1]::Int]
                @assert labelnum !== 0
                el.args[1] = labelnum
            end
        end
    end
end

function linearize!(code::CodeInfo)
    body = code.code
    len = length(body)
    next_i = 1
    stmts = []
    while next_i <= len
        i = next_i
        next_i += 1
        ex = body[i]
        isa(ex, Expr) || continue
        ex = ex::Expr
        head = ex.head
        is_meta_expr_head(head) && continue
        if head === :(=)
            ex = ex.args[2]
            isa(ex, Expr) || continue
            ex = ex::Expr
            head = ex.head
        end
        args = ex.args
        if head === :foreigncall
            if isa(args[1], Expr) && !is_ccall_static(args[1]::Expr)
                linearize_arg!(args, 1, stmts, code)
            end
            for j in 2:length(args)
                a = args[j]
                isa(a, Expr) || continue
                if a.head === :&
                    linearize_arg!(a.args, 1, stmts, code)
                else
                    linearize_arg!(args, j, stmts, code)
                end
            end
        elseif (head === :import || head === :using || head === :importall || head === :export ||
                head === :isdefined || head === :const || is_meta_expr_head(head))
            continue
        elseif head === :call
            if ex.args[1] == Core.Intrinsics.llvmcall
                for j in 5:length(args)
                    linearize_arg!(args, j, stmts, code)
                end
            elseif ex.args[1] == Core.Intrinsics.cglobal
                if isa(args[2], Expr) && !is_ccall_static(args[2]::Expr)
                    linearize_arg!(args, 2, stmts, code)
                end
                for j in 3:length(args)
                    linearize_arg!(args, j, stmts, code)
                end
            else
                for j in 1:length(args)
                    linearize_arg!(args, j, stmts, code)
                end
            end
        else
            for j in 1:length(args)
                if j == 1 && head === :method
                    argj = args[j]
                    if isa(argj, Slot) || isa(argj, Symbol) || isa(argj, GlobalRef)
                        continue
                    end
                end
                linearize_arg!(args, j, stmts, code)
            end
        end
        isempty(stmts) && continue
        next_i = i
        splice!(body, i:(i - 1), stmts)
        len += length(stmts)
        empty!(stmts)
    end
    relabel!(body)
end