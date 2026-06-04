const compiled_calls = Dict{Any,Any}()

# Pre-frame-construction lookup
function lookup_stmt(stmts::Vector{Any}, @nospecialize(arg), world::UInt)
    # this converts a statement into something else, without the slightest interest in correctness:
    if isa(arg, SSAValue)
        arg = stmts[arg.id]
    end
    if isa(arg, QuoteNode)
        return arg.value
    elseif isexpr(arg, :call, 3) && (is_global_ref(arg.args[1], Base, :getproperty) ||
                                     is_quotenode_egal(arg.args[1], Base.getproperty))
        # `getproperty` may have been folded from a `GlobalRef` to a `QuoteNode` by `optimize!`.
        # Starting with Julia 1.12, llvmcall looks like this:
        # julia> src.code[1:3]
        # 3-element Vector{Any}:
        #  :(TheModule.Core)                          # GlobalRef
        #  :(Base.getproperty(%1, :Intrinsics))
        #  :(Base.getproperty(%2, :llvmcall))
        q = arg.args[3]
        if isa(q, QuoteNode) && (qval = q.value; qval isa Symbol)
            mod = lookup_stmt(stmts, arg.args[2], world)
            if isa(mod, GlobalRef)
                if invoke_in_world(world, isdefinedglobal, mod.mod, mod.name)
                    mod = invoke_in_world(world, getglobal, mod.mod, mod.name)
                end
            end
            if isa(mod, Module)
                if invoke_in_world(world, isdefinedglobal, mod, qval)
                    return invoke_in_world(world, getglobal, mod, qval)
                end
            end
        end
    end
    return arg
end

function smallest_ref(stmts, arg, idmin)
    if isa(arg, SSAValue)
        idmin = min(idmin, arg.id)
        return smallest_ref(stmts, stmts[arg.id], idmin)
    elseif isa(arg, Expr)
        for a in arg.args
            idmin = smallest_ref(stmts, a, idmin)
        end
    end
    return idmin
end

@static if isbindingresolved_deprecated
    # Julia 1.12+: a `const` may be redefined, which advances the world age, so a folded value is
    # only valid while its binding partition still covers the frame's world. Record that partition
    # in `world_deps` as an invalidation token (redefinition drops its `max_world` below the
    # redefinition world); `framecode_valid_world` then rejects the cached `FrameCode` for any world
    # a folded value would be stale in, triggering a rebuild.
    function lookup_global_ref(a::GlobalRef, world::UInt, world_deps)
        if invoke_in_world(world, isdefinedglobal, a.mod, a.name) &&
           invoke_in_world(world, isconst, a.mod, a.name)
            partition = Base.lookup_binding_partition(world, a)
            # Only fold *explicit* `const`s, not implicit (`using`-imported) bindings: an implicit
            # name may be locally shadowed — e.g. `sin(::Int) = …` in a module that uses `Base.sin`,
            # where the `:method` defines a new local `sin` — so folding the imported value would
            # bind the method to the wrong function. (Pre-1.12 this role was served by the now-inert
            # `isbindingresolved`.) Genuine local/explicit `const`s are the optimization's real target.
            if !Base.is_some_implicit(Base.binding_kind(partition))
                push!(world_deps, partition)
                return QuoteNode(invoke_in_world(world, getglobal, a.mod, a.name))
            end
        end
        return a
    end
else
    # Pre-1.12: `const` bindings cannot be redefined, so a folded value never goes stale and no
    # invalidation tracking is needed (`world_deps` is left untouched).
    function lookup_global_ref(a::GlobalRef, world::UInt, world_deps)
        if Base.isbindingresolved(a.mod, a.name) &&
            (invoke_in_world(world, isdefinedglobal, a.mod, a.name)) &&
            (invoke_in_world(world, isconst, a.mod, a.name))
            return QuoteNode(invoke_in_world(world, getglobal, a.mod, a.name))
        end
        return a
    end
end

function lookup_global_refs!(ex::Expr, world::UInt, world_deps)
    if isexpr(ex, (:isdefined, :thunk, :toplevel, :method, :global, :const, :globaldecl))
        return nothing
    end
    for (i, a) in enumerate(ex.args)
        ex.head === :(=) && i == 1 && continue # Don't look up globalrefs on the LHS of an assignment (issue #98)
        if isa(a, GlobalRef)
            ex.args[i] = lookup_global_ref(a, world, world_deps)
        elseif isa(a, Expr)
            lookup_global_refs!(a, world, world_deps)
        end
    end
    return nothing
end

function lookup_getproperties(code::Vector{Any}, @nospecialize(a), world::UInt, world_deps)
    isexpr(a, :call) || return a
    length(a.args) == 3 || return a
    arg1 = lookup_stmt(code, a.args[1], world)
    arg1 === Base.getproperty || return a
    arg2 = lookup_stmt(code, a.args[2], world)
    arg2 isa Module || return a
    arg3 = lookup_stmt(code, a.args[3], world)
    arg3 isa Symbol || return a
    return lookup_global_ref(GlobalRef(arg2, arg3), world, world_deps)
end

# HACK This isn't optimization really, but necessary to bypass llvmcall and foreigncall
# TODO This "optimization" should be refactored into a "minimum compilation" necessary to
# execute `llvmcall` and `foreigncall` and pure optimizations on the lowered code representation.
# In particular, the optimization that replaces `GlobalRef` with `QuoteNode` is invalid and
# should be removed: This is because it is not possible to know when and where the binding
# will be resolved without executing the code.
# Since the current `build_compiled_[llvmcall|foreigncall]!` relies on this replacement,
# they also need to be reimplemented.

"""
    optimize!(code::CodeInfo, mod::Module)

Perform minor optimizations on the lowered AST in `code` to reduce execution time
of the interpreter.
Currently it looks up `GlobalRef`s (for which it needs `mod` to know the scope in
which this will run) and ensures that no statement includes nested `:call` expressions
(splitting them out into multiple SSA-form statements if needed).
"""
function optimize!(code::CodeInfo, scope, world::UInt)
    mod = moduleof(scope)
    evalmod = mod == Core.Compiler ? Core.Compiler : CompiledCalls
    sparams = scope isa Method ? sparam_syms(scope) : Symbol[]
    replace_coretypes!(code)

    # Binding partitions for the `const` globals folded below; recorded so a cached `FrameCode`
    # can be invalidated once any folded value goes stale (see `FrameCode.world_deps`).
    world_deps = BindingPartition[]
    # TODO: because of builtins.jl, for CodeInfos like
    #   %1 = Core.apply_type
    #   %2 = (%1)(args...)
    # it would be best to *not* resolve the GlobalRef at %1
    ## Replace GlobalRefs with QuoteNodes
    # On Julia 1.12+ fold only in method scope: a method frame executes in a single fixed
    # world, so a value folded at `world` is exact. A module-scoped (toplevel) thunk advances
    # the world at `:latestworld` markers — a `const` redefined and then read within the same
    # thunk would serve a stale folded value — and such thunks execute once, so folding has
    # nothing to gain there.
    if scope isa Method || !isbindingresolved_deprecated
        for (i, stmt) in enumerate(code.code)
            if isa(stmt, GlobalRef)
                code.code[i] = lookup_global_ref(stmt, world, world_deps)
            elseif isa(stmt, Expr)
                if stmt.head === :call && stmt.args[1] === :cglobal  # cglobal requires literals
                    continue
                else
                    lookup_global_refs!(stmt, world, world_deps)
                    code.code[i] = lookup_getproperties(code.code, stmt, world, world_deps)
                end
            end
        end
    end

    # Replace :llvmcall and :foreigncall with compiled variants. See
    # https://github.com/JuliaDebug/JuliaInterpreter.jl/issues/13#issuecomment-464880123
    # Insert the foreigncall wrappers at the updated idxs
    methodtables = Vector{Union{Compiled,DispatchableMethod}}(undef, length(code.code))
    for (idx, stmt) in enumerate(code.code)
        # Foregincalls can be rhs of assignments
        if isexpr(stmt, :(=))
            stmt = (stmt::Expr).args[2]
        end
        if isa(stmt, Expr)
            if stmt.head === :call
                # Check for :llvmcall
                arg1 = stmt.args[1]
                larg1 = lookup_stmt(code.code, arg1, world)
                if (arg1 === :llvmcall || larg1 === Base.llvmcall || is_global_ref_egal(larg1, :llvmcall, Core.Intrinsics.llvmcall)) && isempty(sparams) && scope isa Method
                    # Call via `invokelatest` to avoid compiling it until we need it
                    @invokelatest build_compiled_llvmcall!(stmt, code, idx, evalmod, world)
                    methodtables[idx] = Compiled()
                end
            elseif stmt.head === :foreigncall && scope isa Method
                # Call via `invokelatest` to avoid compiling it until we need it
                @invokelatest build_compiled_foreigncall!(stmt, code, sparams, evalmod, world)
                methodtables[idx] = Compiled()
            end
        end
    end

    return code, methodtables, world_deps
end

function parametric_type_to_expr(@nospecialize(t::Type))
    t isa Core.TypeofBottom && return t
    while t isa UnionAll
        t = t.body
    end
    t = t::DataType
    if Base.isvarargtype(t)
        return Expr(:(...), t.parameters[1])
    end
    if Base.has_free_typevars(t)
        params = map(t.parameters) do @nospecialize(p)
            isa(p, TypeVar) ? p.name :
            isa(p, DataType) && Base.has_free_typevars(p) ? parametric_type_to_expr(p) : p
        end
        return Expr(:curly, scopename(t.name), params...)::Expr
    end
    return t
end

function build_compiled_llvmcall!(stmt::Expr, code::CodeInfo, idx::Int, evalmod::Module, world::UInt)
    # Run a mini-interpreter to extract the types
    framecode = FrameCode(CompiledCalls, code; optimize=false, world)
    frame = Frame(framecode, prepare_framedata(framecode, []), 1, nothing, world)
    idxstart = idx
    for i = 2:4
        idxstart = smallest_ref(code.code, stmt.args[i], idxstart)
    end
    frame.pc = idxstart
    if idxstart < idx
        while true
            pc = step_expr!(NonRecursiveInterpreter(), frame)
            pc === idx && break
            pc === nothing && error("this should never happen")
        end
    end
    llvmir, RetType, ArgType = lookup(frame, stmt.args[2]), lookup(frame, stmt.args[3]), lookup(frame, stmt.args[4])::DataType
    args = stmt.args[5:end]
    argnames = Any[Symbol(:arg, i) for i = 1:length(args)]
    cc_key = (llvmir, RetType, ArgType, evalmod)  # compiled call key
    f = get(compiled_calls, cc_key, nothing)
    if f === nothing
        methname = gensym("compiled_llvmcall")
        def = :(
            function $methname($(argnames...))
                return $(Base.llvmcall)($llvmir, $RetType, $ArgType, $(argnames...))
            end)
        f = Core.eval(evalmod, def)
        compiled_calls[cc_key] = f
    end

    stmt.args[1] = QuoteNode(f)
    stmt.head = :call
    deleteat!(stmt.args, 2:length(stmt.args))
    append!(stmt.args, args)
end

# Handle :llvmcall & :foreigncall (issue #28)
function build_compiled_foreigncall!(stmt::Expr, code::CodeInfo, sparams::Vector{Symbol}, evalmod::Module, world::UInt)
    TVal = evalmod == Core.Compiler ? Core.Compiler.Val : Val
    RetType, ArgType = stmt.args[2], stmt.args[3]::SimpleVector

    dynamic_ccall = false
    argcfunc = cfunc = stmt.args[1]
    if @isdefined(__has_internal_change) && __has_internal_change(v"1.13.0", :syntacticccall)
        if !isexpr(cfunc, :tuple)
            dynamic_ccall = true
            cfunc = gensym("ptr")
        end
    else
        while isa(cfunc, SSAValue)
            cfunc = lookup_stmt(code.code, cfunc, world)
            cfunc isa Symbol && (cfunc = QuoteNode(cfunc))
        end
        # n.b. Base.memhash is deprecated (continued use would cause serious faults) in the same version as the syntax is deprecated
        # so this is only needed as a legacy hack
        if isa(cfunc, Expr) || (cfunc isa GlobalRef && cfunc == GlobalRef(Base, :memhash))
            cfunc = something(try QuoteNode(Core.eval(evalmod, cfunc)) catch nothing end, cfunc)
        end
        if !(isa(cfunc, Union{String, Tuple}) || (isa(cfunc, QuoteNode) && isa(cfunc.value, Union{String, Tuple, Symbol})))
            dynamic_ccall = true
            cfunc = gensym("ptr")
        end
    end

    if isa(RetType, SimpleVector)
        @assert length(RetType) == 1
        RetType = RetType[1]
    end
    args = stmt.args[6:end]
    # When the ccall is dynamic we pass the pointer as an argument so can reuse the function
    cc_key = ((dynamic_ccall ? :ptr : cfunc), RetType, ArgType, evalmod, length(sparams), length(args))  # compiled call key
    f = get(compiled_calls, cc_key, nothing)
    if f === nothing
        ArgType = Expr(:tuple, Any[parametric_type_to_expr(t) for t in ArgType::SimpleVector]...)
        RetType = parametric_type_to_expr(RetType)
        # #285: test whether we can evaluate an type constraints on parametric expressions
        # this essentially comes down to having the names be available in CompiledCalls,
        # if they are not then executing the method will fail
        try
            isa(RetType, Expr) && Core.eval(CompiledCalls, wrap_params(RetType, sparams))
            isa(ArgType, Expr) && Core.eval(CompiledCalls, wrap_params(ArgType, sparams))
        catch
            return nothing
        end
        argnames = Any[Symbol(:arg, i) for i = 1:length(args)]
        wrapargs = copy(argnames)
        for sparam in sparams
            push!(wrapargs, :(::$TVal{$sparam}))
        end
        if dynamic_ccall
            pushfirst!(wrapargs, cfunc)
        end
        methname = gensym("compiled_ccall")
        def = :(
            function $methname($(wrapargs...)) where {$(sparams...)}
                return $(Expr(:foreigncall, cfunc, RetType, stmt.args[3:5]..., argnames...))
            end)
        f = Core.eval(evalmod, def)
        compiled_calls[cc_key] = f
    end
    stmt.args[1] = QuoteNode(f)
    stmt.head = :call
    deleteat!(stmt.args, 2:length(stmt.args))
    if dynamic_ccall
        push!(stmt.args, argcfunc)
    end
    append!(stmt.args, args)
    for i in 1:length(sparams)
        push!(stmt.args, :($TVal($(Expr(:static_parameter, i)))))
    end
    return nothing
end

function replace_coretypes!(@nospecialize(src); rev::Bool=false)
    if isa(src, CodeInfo)
        replace_coretypes_list!(src.code; rev=rev)
    elseif isa(src, Expr)
        replace_coretypes_list!(src.args; rev=rev)
    end
    return src
end

function replace_coretypes_list!(list::AbstractVector; rev::Bool=false)
    function rep(@nospecialize(x), rev::Bool)
        if rev
            isa(x, SSAValue) && return Core.SSAValue(x.id)
            isa(x, SlotNumber) && return Core.SlotNumber(x.id)
            return x
        else
            isa(x, Core.SSAValue) && return SSAValue(x.id)
            isa(x, Core.SlotNumber) && return SlotNumber(x.id)
            isa(x, Core.Compiler.Argument) && return SlotNumber(x.n)
            @static if VERSION < v"1.11.0-DEV.337"
            isa(x, Core.Compiler.TypedSlot) && return SlotNumber(x.id)
            end
            return x
        end
    end

    for (i, stmt) in enumerate(list)
        rstmt = rep(stmt, rev)
        if rstmt !== stmt
            list[i] = rstmt
        elseif isa(stmt, GotoIfNot)
            cond = stmt.cond
            rcond = rep(cond, rev)
            if rcond !== cond
                list[i] = GotoIfNot(rcond, stmt.dest)
            end
        elseif isa(stmt, ReturnNode)
            val = stmt.val
            rval = rep(val, rev)
            if rval !== val
                list[i] = ReturnNode(rval)
            end
        elseif @static (isdefinedglobal(Core.IR, :EnterNode) && true) && isa(stmt, Core.IR.EnterNode)
            if isdefined(stmt, :scope)
                rscope = rep(stmt.scope, rev)
                if rscope !== stmt.scope
                    list[i] = Core.IR.EnterNode(stmt.catch_dest, rscope)
                end
            end
        elseif isa(stmt, Expr)
            replace_coretypes!(stmt; rev=rev)
        end
    end
    return nothing
end

function reverse_lookup_globalref!(list)
    # This only handles the function in calls
    for (i, stmt) in enumerate(list)
        if isexpr(stmt, :(=))
            stmt = (stmt::Expr).args[2]
        end
        if isexpr(stmt, :call)
            stmt = stmt::Expr
            f = stmt.args[1]
            if isa(f, QuoteNode)
                f = f.value
                if isa(f, Function) && !isa(f, Core.IntrinsicFunction)
                    ft = typeof(f)
                    tn = ft.name::Core.TypeName
                    name = String(tn.name)
                    if startswith(name, '#')
                        name = name[2:end]
                    end
                    stmt.args[1] = GlobalRef(tn.module, Symbol(name))
                end
            end
        end
    end
    return list
end
