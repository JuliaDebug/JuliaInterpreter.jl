## Simple utils

# Note: to avoid dynamic dispatch, many of these are coded as a single method using isa statements

function scopeof(@nospecialize(x))::Union{Method,Module}
    (isa(x, Method) || isa(x, Module)) && return x
    isa(x, FrameCode) && return x.scope
    isa(x, Frame) && return x.framecode.scope
    error("unknown scope for ", x)
end

function moduleof(@nospecialize(x))
    s = scopeof(x)
    return isa(s, Module) ? s : s.module
end

function Base.nameof(frame::Frame)
    s = frame.framecode.scope
    isa(s, Method) ? s.name : nameof(s)
end

_Typeof(x) = isa(x, Type) ? Type{x} : typeof(x)

function to_function(@nospecialize(x))
    isa(x, GlobalRef) ? getfield(x.mod, x.name) : x
end

"""
    method = whichtt(tt)

Like `which` except it operates on the complete tuple-type `tt`.
"""
function whichtt(@nospecialize(tt))
    # TODO: provide explicit control over world age? In case we ever need to call "old" methods.
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
pc_expr(frame::Frame, pc) = pc_expr(frame.framecode, pc)
pc_expr(frame::Frame) = pc_expr(frame, frame.pc)

function find_used(code::CodeInfo)
    used = BitSet()
    stmts = code.code
    for stmt in stmts
        scan_ssa_use!(used, stmt)
        if isexpr(stmt, :struct_type)  # this one is missed by Core.Compiler.userefs
            for a in stmt.args
                scan_ssa_use!(used, a)
            end
        end
    end
    return used
end

function scan_ssa_use!(used::BitSet, @nospecialize(stmt))
    if isa(stmt, SSAValue)
        push!(used, stmt.id)
    end
    iter = Core.Compiler.userefs(stmt)
    iterval = Core.Compiler.iterate(iter)
    while iterval !== nothing
        useref, state = iterval
        val = Core.Compiler.getindex(useref)
        if isa(val, SSAValue)
            push!(used, val.id)
        end
        iterval = Core.Compiler.iterate(iter, state)
    end
end

function hasarg(predicate, args)
    predicate(args) && return true
    for a in args
        predicate(a) && return true
        if isa(a, Expr)
            hasarg(predicate, a.args) && return true
        elseif isa(a, QuoteNode)
            predicate(a.value) && return true
        elseif isa(a, GlobalRef)
            predicate(a.name) && return true
        end
    end
    return false
end

## Predicates

is_goto_node(@nospecialize(node)) = isa(node, GotoNode) || isexpr(node, :gotoifnot)

is_loc_meta(@nospecialize(expr), @nospecialize(kind)) = isexpr(expr, :meta) && length(expr.args) >= 1 && expr.args[1] === kind

"""
    is_global_ref(g, mod, name)

Tests whether `g` is equal to `GlobalRef(mod, name)`.
"""
is_global_ref(@nospecialize(g), mod::Module, name::Symbol) = isa(g, GlobalRef) && g.mod === mod && g.name == name

is_quotenode(@nospecialize(q), @nospecialize(val)) = isa(q, QuoteNode) && q.value == val

function is_quoted_type(@nospecialize(a), name::Symbol)
    if isa(a, QuoteNode)
        T = a.value
        isa(T, Type) || return false
        if isa(T, UnionAll)
            T = Base.unwrap_unionall(T)
        end
        return T.name.name == :NamedTuple
    end
    return false
end

function is_function_def(@nospecialize(ex))
    (isexpr(ex, :(=)) && isexpr(ex.args[1], :call)) || isexpr(ex, :function)
end

function is_call(@nospecialize(node))
    isexpr(node, :call) ||
        (isexpr(node, :(=)) && (isexpr(node.args[2], :call)))
end

is_call_or_return(@nospecialize(node)) = is_call(node) || isexpr(node, :return)

is_dummy(bpref::BreakpointRef) = bpref.stmtidx == 0 && bpref.err === nothing

"""
Determine whether we are calling a function for which the current function
is a wrapper (either because of optional arguments or because of keyword arguments).
"""
function is_wrapper_call(@nospecialize(expr))
    isexpr(expr, :(=)) && (expr = expr.args[2])
    isexpr(expr, :call) && any(x->x==SlotNumber(1), expr.args)
end

is_generated(meth::Method) = isdefined(meth, :generator)

"""
    is_doc_expr(ex)

Test whether expression `ex` is a `@doc` expression.
"""
function is_doc_expr(@nospecialize(ex))
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

is_leaf(frame::Frame) = frame.callee === nothing

function is_vararg_type(x)
    if isa(x, Type)
        (x <: Vararg && !(x <: Union{})) && return true
        if isa(x, UnionAll)
            x = Base.unwrap_unionall(x)
        end
        return isa(x, DataType) && nameof(x) == :Vararg
    end
    return false
end

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

getline(ln) = isexpr(ln, :line) ? ln.args[1] : ln.line
getfile(ln) = CodeTracking.maybe_fixup_stdlib_path(String(isexpr(ln, :line) ? ln.args[2] : ln.file))

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
        whereis(lineinfo, framecode.scope) : (getfile(lineinfo), getline(lineinfo))
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
    return getline(framecode.src.linetable[codeloc])
end
linenumber(frame::Frame, pc=frame.pc) = linenumber(frame.framecode, pc)

function getfile(framecode::FrameCode, pc)
    codeloc = codelocation(framecode.src, pc)
    codeloc == 0 && return nothing
    return getfile(framecode.src.linetable[codeloc])
end
getfile(frame::Frame, pc=frame.pc) = getfile(frame.framecode, pc)

function codelocation(code::CodeInfo, idx)
    codeloc = code.codelocs[idx]
    while codeloc == 0 && code.code[idx] === nothing && idx < length(code.code)
        idx += 1
        codeloc = code.codelocs[idx]
    end
    return codeloc
end

function compute_corrected_linerange(method::Method)
    _, line1 = whereis(method)
    offset = line1 - method.line
    src = JuliaInterpreter.get_source(method)
    lastline = src.linetable[end]
    return line1:getline(lastline) + offset
end

function method_contains_line(method::Method, line::Integer)
    # Check to see if this method really contains that line. Methods that fill in a default positional argument,
    # keyword arguments, and @generated sections may not contain the line.
    return line in compute_corrected_linerange(method)
end

"""
    stmtidx = statementnumber(frame, line)

Return the index of the first statement in `frame`'s `CodeInfo` that corresponds to
static line number `line`.
"""
function statementnumber(framecode::FrameCode, line)
    lineidx = searchsortedfirst(framecode.src.linetable, line; by=lin->isa(lin,Integer) ? lin : getline(lin))
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
        !(line == "CodeInfo(" || line == ")" || isempty(line) || occursin("within `", line))
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
    ndline = isempty(lt) ? 0 : ndigits(getline(lt[end]) + offset)
    nullline = " "^ndline
    src = copy_codeinfo(framecode.src)
    replace_coretypes!(src; rev=true)
    code = framecode_lines(src)
    isfirst = true
    for (stmtidx, stmtline) in enumerate(code)
        stmtidx ∈ range || continue
        bpc = breakpointchar(framecode, stmtidx)
        isfirst || print(io, '\n')
        isfirst = false
        print(io, bpc, ' ')
        if iscolor
            color = stmtidx == pc ? Base.warn_color() : :normal
            printstyled(io, lpad(stmtidx, ndstmt); color=color, kwargs...)
        else
            print(io, lpad(stmtidx, ndstmt), stmtidx == pc ? '*' : ' ')
        end
        line = linenumber(framecode, stmtidx)
        print(io, ' ', line === nothing ? nullline : lpad(line, ndline), "  ", stmtline)
    end
end

"""
    local_variables = locals(frame::Frame)::Vector{Variable}

Return the local variables as a vector of [`Variable`](@ref).
"""
function locals(frame::Frame)
    vars, var_counter = Variable[], Int[]
    varlookup = Dict{Symbol,Int}()
    data, code = frame.framedata, frame.framecode
    slotnames = code.src.slotnames::SlotNamesType
    for (sym, counter, val) in zip(slotnames, data.last_reference, data.locals)
        counter == 0 && continue
        var = Variable(something(val), sym, false)
        idx = get(varlookup, sym, 0)
        if idx > 0
            if counter > var_counter[idx]
                vars[idx] = var
                var_counter[idx] = counter
            end
        else
            varlookup[sym] = length(vars)+1
            push!(vars, var)
            push!(var_counter, counter)
        end
    end
    if code.scope isa Method
        syms = sparam_syms(code.scope)
        for i in 1:length(syms)
            if isassigned(data.sparams, i)
                push!(vars, Variable(data.sparams[i], syms[i], true))
            end
        end
    end
    return vars
end

function print_vars(io::IO, vars::Vector{Variable})
    for v in vars
        v.name == Symbol("#self#") && (isa(v.value, Type) || sizeof(v.value) == 0) && continue
        print(io, '\n', v)
    end
end

"""
    eval_code(frame::Frame, code::Union{String, Expr})

Evaluate `code` in the context of `frame`, updating any local variables
(including type parameters) that are reassigned in `code`, however, new local variables
cannot be introduced.

```jldoctest
julia> foo(x, y) = x + y;

julia> frame = JuliaInterpreter.enter_call(foo, 1, 3);

julia> JuliaInterpreter.eval_code(frame, "x + y")
4

julia> JuliaInterpreter.eval_code(frame, "x = 5");

julia> JuliaInterpreter.finish_and_return!(frame)
8
```

When variables are captured in closures (and thus gets wrapped in a `Core.Box`)
they will be automatically unwrapped and rewrapped upon evaluating them:

```jldoctest
julia> function capture()
           x = 1
           f = ()->(x = 2) # x captured in closure and is thus a Core.Box
           f()
           x
       end;

julia> frame = JuliaInterpreter.enter_call(capture);

julia> JuliaInterpreter.step_expr!(frame);

julia> JuliaInterpreter.step_expr!(frame);

julia> JuliaInterpreter.locals(frame)
2-element Array{JuliaInterpreter.Variable,1}:
 #self# = capture
 x = Core.Box(1)

julia> JuliaInterpreter.eval_code(frame, "x")
1

julia> JuliaInterpreter.eval_code(frame, "x = 2")
2

julia> JuliaInterpreter.locals(frame)
2-element Array{JuliaInterpreter.Variable,1}:
 #self# = capture
 x = Core.Box(2)
```
"""
function eval_code end

eval_code(frame::Frame, command::AbstractString) = eval_code(frame, Base.parse_input_line(command))
function eval_code(frame::Frame, expr)
    maybe_quote(x) = (isa(x, Expr) || isa(x, Symbol)) ? QuoteNode(x) : x
    code = frame.framecode
    data = frame.framedata
    isexpr(expr, :toplevel) && (expr = expr.args[end])

    if isexpr(expr, :toplevel)
      expr = Expr(:block, expr.args...)
    end
    # see https://github.com/JuliaLang/julia/issues/31255 for the Symbol("") check
    vars = filter(v -> v.name != Symbol(""), locals(frame))
    res = gensym()
    eval_expr = Expr(:let,
        Expr(:block, map(x->Expr(:(=), x...), [(v.name, maybe_quote(v.value isa Core.Box ? v.value.contents : v.value)) for v in vars])...),
        Expr(:block,
            Expr(:(=), res, expr),
            Expr(:tuple, res, Expr(:tuple, [v.name for v in vars]...))
        ))
    eval_res, res = Core.eval(moduleof(frame), eval_expr)
    j = 1
    for (i, v) in enumerate(vars)
        if v.isparam
            data.sparams[j] = res[i]
            j += 1
        else
            slot_indices = code.slotnamelists[v.name]
            idx = argmax(data.last_reference[slot_indices])
            slot_idx = slot_indices[idx]
            data.last_reference[slot_idx] = (frame.assignment_counter += 1)
            data.locals[slot_idx] = Some{Any}(v.value isa Core.Box ? Core.Box(res[i]) : res[i])
        end
    end
    eval_res
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

# Printing of stacktraces and errors with Frame
function Base.StackTraces.StackFrame(frame::Frame)
    if scopeof(frame) isa Method
        method = frame.framecode.scope
        method_args = something.(frame.framedata.locals[1:method.nargs])
        atypes = Tuple{typeof.(method_args)...}
        sig = method.sig
        sparams = Core.svec(frame.framedata.sparams...)
        mi = specialize_method(method, atypes, sparams)
    else
        mi = frame.framecode.src
    end
    Base.StackFrame(
        frame.framecode.scope.name,
        Symbol(JuliaInterpreter.getfile(frame)),
        JuliaInterpreter.linenumber(frame),
        mi,
        false,
        false,
        C_NULL
    )
end

function Base.show_backtrace(io::IO, frame::Frame)
    stackframes = Tuple{Base.StackTraces.StackFrame, Int}[]
    while frame !== nothing
        push!(stackframes, (Base.StackTraces.StackFrame(frame), 1))
        frame = JuliaInterpreter.caller(frame)
    end
    print(io, "\nStacktrace:")
    try invokelatest(Base.update_stackframes_callback[], stackframes) catch end
    frame_counter = 0
    for (last_frame, n) in stackframes
        frame_counter += 1
        Base.show_trace_entry(IOContext(io, :backtrace => true), last_frame, n, prefix = string(" [", frame_counter, "] "))
    end
end

function Base.display_error(io::IO, er, frame::Frame)
    printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
    showerror(IOContext(io, :limit => true), er, frame)
    println(io)
end
