## Simple utils

# Note: to avoid dynamic dispatch, many of these are coded as a single method using isa statements

function scopeof(x)::Union{Method,Module}
    (isa(x, Method) || isa(x, Module)) && return x
    isa(x, FrameCode) && return x.scope
    isa(x, Frame) && return x.framecode.scope
    error("unknown scope for ", x)
end

function moduleof(x)
    s = scopeof(x)
    return isa(s, Module) ? s : s.module
end

function Base.nameof(frame::Frame)
    s = frame.framecode.scope
    isa(s, Method) ? s.name : nameof(s)
end

_Typeof(x) = isa(x,Type) ? Type{x} : typeof(x)

function to_function(x)
    isa(x, GlobalRef) ? getfield(x.mod, x.name) : x
end

function whichtt(tt)
    m = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), tt, typemax(UInt))
    m === nothing && return nothing
    return m.func::Method
end

instantiate_type_in_env(arg, spsig, spvals) =
    ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), arg, spsig, spvals)

function sparam_syms(meth::Method)
    s = Symbol[]
    sig = meth.sig
    while sig isa UnionAll
        push!(s, Symbol(sig.var.name))
        sig = sig.body
    end
    return s
end

separate_kwargs(args...; kwargs...) = (args, kwargs.data)

pc_expr(src::CodeInfo, pc) = src.code[pc]
pc_expr(framecode::FrameCode, pc) = pc_expr(framecode.src, pc)
pc_expr(frame, pc) = pc_expr(frame.framecode, pc)
pc_expr(frame) = pc_expr(frame, frame.pc)

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

## Predicates

is_goto_node(node) = isa(node, GotoNode) || isexpr(node, :gotoifnot)

is_loc_meta(expr, kind) = isexpr(expr, :meta) && length(expr.args) >= 1 && expr.args[1] === kind

"""
    is_global_ref(g, mod, name)

Tests whether `g` is equal to `GlobalRef(mod, name)`.
"""
is_global_ref(g, mod, name) = isa(g, GlobalRef) && g.mod === mod && g.name == name

function is_function_def(ex)
    (isexpr(ex, :(=)) && isexpr(ex.args[1], :call)) ||
    isexpr(ex,:function)
end

function is_call(node)
    isexpr(node, :call) ||
    (isexpr(node, :(=)) && (isexpr(node.args[2], :call)))
end

is_call_or_return(node) = is_call(node) || isexpr(node, :return)

is_dummy(bpref::BreakpointRef) = bpref.stmtidx == 0 && bpref.err === nothing

"""
Determine whether we are calling a function for which the current function
is a wrapper (either because of optional arguments or because of keyword arguments).
"""
function is_wrapper_call(expr)
    isexpr(expr, :(=)) && (expr = expr.args[2])
    isexpr(expr, :call) && any(x->x==SlotNumber(1), expr.args)
end

function is_generated(meth)
    isdefined(meth, :generator)
end

"""
    is_doc_expr(ex)

Test whether expression `ex` is a `@doc` expression.
"""
function is_doc_expr(ex)
    docsym = Symbol("@doc")
    if isexpr(ex, :macrocall)
        a = ex.args[1]
        is_global_ref(a, Core, docsym) && return true
        isa(a, Symbol) && a == docsym && return true
        if isexpr(a, :.)
            mod, name = a.args[1], a.args[2]
            return mod === :Core && isa(name, QuoteNode) && name.value == docsym
        end
    end
    return false
end

is_leaf(frame) = frame.callee === nothing

## Location info

function lineoffset(framecode::FrameCode)
    offset = 0
    scope = framecode.scope
    if isa(scope, Method)
        _, line1 = whereis(scope)
        offset = line1 - scope.line
    end
    return offset
end

"""
    loc = whereis(frame, pc=frame.pc)

Return the file and line number for `frame` at `pc`.  If this cannot be
determined, `loc == nothing`. Otherwise `loc == (filepath, line)`.
"""
function CodeTracking.whereis(framecode::FrameCode, pc)
    codeloc = codelocation(framecode.src, pc)
    codeloc == 0 && return nothing
    lineinfo = framecode.src.linetable[codeloc]
    return isa(framecode.scope, Method) ?
        whereis(lineinfo, framecode.scope) : string(lineinfo.file), lineinfo.line
end
CodeTracking.whereis(frame::Frame, pc=frame.pc) = whereis(frame.framecode, pc)

"""
    line = linenumber(framecode, pc)

Return the "static" line number at statement index `pc`. The static line number
is the location at the time the method was most recently defined.
See [`CodeTracking.whereis`](@ref) for dynamic line information.
"""
function linenumber(framecode::FrameCode, pc)
    codeloc = codelocation(framecode.src, pc)
    codeloc == 0 && return nothing
    return framecode.src.linetable[codeloc].line
end
linenumber(frame::Frame, pc=frame.pc) = linenumber(frame.framecode, pc)

function codelocation(code::CodeInfo, idx)
    codeloc = code.codelocs[idx]
    while codeloc == 0 && code.code[idx] === nothing && idx < length(code.code)
        idx += 1
        codeloc = code.codelocs[idx]
    end
    return codeloc
end

"""
    stmtidx = statementnumber(frame, line)

Return the index of the first statement in `frame`'s `CodeInfo` that corresponds to
static line number `line`.
"""
function statementnumber(framecode::FrameCode, line)
    lineidx = searchsortedfirst(framecode.src.linetable, line; by=lin->isa(lin,Integer) ? lin : lin.line)
    1 <= lineidx <= length(framecode.src.linetable) || throw(ArgumentError("line $line not found in $(framecode.scope)"))
    return searchsortedfirst(framecode.src.codelocs, lineidx)
end
statementnumber(frame::Frame, line) = statementnumber(frame.framecode, line)

"""
    framecode, stmtidx = statementnumber(method, line)

Return the index of the first statement in `framecode` that corresponds to the given
static line number `line` in `method`.
"""
function statementnumber(method::Method, line; line1=whereis(method)[2])
    linec = line - line1 + method.line  # line number at time of compilation
    framecode = get_framecode(method)
    return framecode, statementnumber(framecode, linec)
end

## Printing

function framecode_lines(src::CodeInfo)
    buf = IOBuffer()
    show(buf, src)
    code = filter!(split(String(take!(buf)), '\n')) do line
        !(line == "CodeInfo(" || line == ")" || isempty(line))
    end
    code .= replace.(code, Ref(r"\$\(QuoteNode\((.+?)\)\)" => s"\1"))
    return code
end
framecode_lines(framecode::FrameCode) = framecode_lines(framecode.src)

breakpointchar(framecode, stmtidx) =
    isassigned(framecode.breakpoints, stmtidx) ? breakpointchar(framecode.breakpoints[stmtidx]) : ' '

function print_framecode(io::IO, framecode::FrameCode; pc=0, range=1:nstatements(framecode), kwargs...)
    iscolor = get(io, :color, false)
    ndstmt = ndigits(nstatements(framecode))
    lt = framecode.src.linetable
    offset = lineoffset(framecode)
    ndline = isempty(lt) ? 0 : ndigits(lt[end].line + offset)
    nullline = " "^ndline
    code = framecode_lines(framecode)
    for (stmtidx, stmtline) in enumerate(code)
        stmtidx ∈ range || continue
        bpc = breakpointchar(framecode, stmtidx)
        print(io, bpc, ' ')
        if iscolor
            color = stmtidx == pc ? Base.warn_color() : :normal
            printstyled(io, lpad(stmtidx, ndstmt); color=color, kwargs...)
        else
            print(io, lpad(stmtidx, ndstmt), stmtidx == pc ? '*' : ' ')
        end
        line = linenumber(framecode, stmtidx)
        println(io, ' ', line === nothing ? nullline : lpad(line, ndline), "  ", stmtline)
    end
end

"""
    local_variables = locals(frame::Frame)::Vector{Variable}

Return the local variables as a vector of `Variable`[@ref].
"""
function locals(frame::Frame)
    vars = Variable[]
    data, code = frame.framedata, frame.framecode
    for (sym, idx) in data.last_reference
        push!(vars, Variable(something(data.locals[idx]), sym, false))
    end
    reverse!(vars)
    if code.scope isa Method
        syms = sparam_syms(code.scope)
        for i in 1:length(syms)
            push!(vars, Variable(data.sparams[i], syms[i], true))
        end
    end
    return vars
end

function show_stackloc(io::IO, frame)
    indent = ""
    fr = root(frame)
    shown = false
    while fr !== nothing
        print(io, indent, scopeof(fr))
        if fr === frame
            println(io, ", pc = ", frame.pc)
            shown = true
        else
            print(io, '\n')
        end
        indent *= "  "
        fr = fr.callee
    end
    if !shown
        println(io, indent, scopeof(frame), ", pc = ", frame.pc)
    end
end
show_stackloc(frame) = show_stackloc(stdout, frame)
