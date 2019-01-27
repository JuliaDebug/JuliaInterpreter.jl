const max_methods = 4  # maximum number of MethodInstances tracked for a particular :call statement

"""
    framecode, lenv = get_call_framecode(fargs, parentframe::JuliaFrameCode, idx::Int)

Return the framecode and environment for a call specified by `fargs = [f, args...]` (see [`prepare_args`](@ref)).
`parentframecode` is the caller, and `idx` is the program-counter index.
If possible, `framecode` will be looked up from the local method tables of `parentframe`.
"""
function get_call_framecode(fargs, parentframe::JuliaFrameCode, idx::Int)
    nargs = length(fargs)  # includes f as the first "argument"
    # Determine whether we can look up the appropriate framecode in the local method table
    if isassigned(parentframe.methodtables, idx)  # if this is the first call, this may not yet be set
        tme = tme1 = parentframe.methodtables[idx]
        local tmeprev
        depth = 1
        while true
            # TODO: consider using world age bounds to handle cache invalidation
            # Determine whether the argument types match the signature
            sig = tme.sig.parameters::SimpleVector
            if length(sig) == nargs
                matches = true
                for i = 1:nargs
                    if !isa(fargs[i], sig[i])
                        matches = false
                        break
                    end
                end
                if matches
                    # Rearrange the list to place this method first
                    # (if we're in a loop, we'll likely match this one again on the next iteration)
                    if depth > 1
                        parentframe.methodtables[idx] = tme
                        tmeprev.next = tme.next
                        tme.next = tme1
                    end
                    # The framecode is stashed in the `inferred` field of the MethodInstance
                    mi = tme.func::MethodInstance
                    return mi.inferred::JuliaFrameCode, mi.sparam_vals
                end
            end
            depth += 1
            tmeprev = tme
            tme = tme.next
            tme === nothing && break
            tme = tme::TypeMapEntry
        end
    end
    # We haven't yet encountered this argtype combination and need to look it up by dispatch
    fargs[1] = f = to_function(fargs[1])
    if isa(f, Core.Builtin)
        # See TODO in optimize!
        return f(fargs[2:end]...), nothing  # for code that has a direct call to a builtin
    end
    # HACK: don't recurse into inference. Inference sometimes returns SSAValue objects and this
    # seems to confuse lookup_var.
    if f === Base._return_type
        return Base._return_type(fargs[2:end]...), nothing
    end
    framecode, args, env, argtypes = prepare_call(f, fargs)
    # Store the results of the method lookup in the local method table
    tme = ccall(:jl_new_struct_uninit, Any, (Any,), TypeMapEntry)::TypeMapEntry
    tme.func = mi = ccall(:jl_new_struct_uninit, Any, (Any,), MethodInstance)::MethodInstance
    tme.sig = mi.specTypes = argtypes
    tme.isleafsig = true
    tme.issimplesig = false
    method = framecode.scope::Method
    tme.va = method.isva
    mi.def = method
    mi.rettype = Any
    mi.sparam_vals = env
    mi.inferred = framecode   # a slight abuse, but not insane
    if isassigned(parentframe.methodtables, idx)
        tme.next = parentframe.methodtables[idx]
        # Drop the oldest tme, if necessary
        tmetmp = tme.next
        depth = 2
        while isdefined(tmetmp, :next) && tmetmp.next !== nothing
            depth += 1
            tmetmp = tmetmp.next
            depth >= max_methods && break
        end
        if depth >= max_methods
            tmetmp.next = nothing
        end
    else
        tme.next = nothing
    end
    parentframe.methodtables[idx] = tme
    return framecode, env
end
