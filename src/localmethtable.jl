const max_methods = 4

# For efficient call-site method tables, we are constrained by the fact that `isa(x, T)`
# is fast only if `T` is a concrete type. The penalty for non-concrete types can be very
# large: for example, `isa(x, Int)` takes ~0.5ns while `isa(x, Integer)` takes ~50ns.
# Consequently it only makes sense to build call-site method tables around concrete types.
# That means the same method may appear in multiple MethodData objects in the table.

struct MethodData
    argtypes::SimpleVector   # argument types
    env::SimpleVector        # type parameters
    framecode::JuliaFrameCode
end

struct LocalMethodTable
    knownmethods::Vector{MethodData}
    callexpr::Expr
end

LocalMethodTable(callexpr::Expr) = LocalMethodTable(MethodData[], callexpr)

Base.show(io::IO, lmt::LocalMethodTable) = print(io, "LocalMethodTable(", lmt.callexpr, ")")
# The following sortof fixes up the display of CodeInfos with LocalMethodTables.
# We still miss usage analysis for SSAValues that are inside expressions like
#    item = LocalMethodTable(Base.getindex(%7, idx))
# I worry, though, about slowing down the compiler (this forces it to use dynamic dispatch).
# Because of this, and because it doesn't fully fix the problem, this is disabled.
# Core.Compiler.scan_ssa_use!(push!, used, lmt::LocalMethodTable) =
#     Core.Compiler.scan_ssa_use!(push!, used, plain(lmt))

function get_framecode(lmt::LocalMethodTable, allargs)
    nargs = length(allargs)
    # knownmethods stored as an LRU cache, where the most recently used is last
    for j = length(lmt.knownmethods):-1:1
        md = lmt.knownmethods[j]
        if length(md.argtypes) == nargs
            matches = true
            for i = 2:nargs  # skip the function itself, which here is still encoded as an expression
                if !isa(allargs[i], md.argtypes[i])
                    matches = false
                    break
                end
            end
            matches || continue
            # Preserve the LRU cache by ensuring md is last
            for k = j:length(lmt.knownmethods)-1
                lmt.knownmethods[k] = lmt.knownmethods[k+1]
            end
            lmt.knownmethods[end] = md
            return md.framecode, md.env
        end
    end
    # We haven't yet encountered this argtype combination and need to look it up
    allargs[1] = f = to_function(allargs[1])
    framecode, args, env, argtypes = prepare_call(f, allargs)
    # Store the results of the method lookup
    md = MethodData(argtypes.parameters, env, framecode)
    if length(lmt.knownmethods) < max_methods
        push!(lmt.knownmethods, md)
    else
        # Discard the least-recently-used
        for i = 1:max_methods-1
            lmt.knownmethods[i] = lmt.knownmethods[i+1]
        end
        lmt.knownmethods[end] = md
    end
    return framecode, env
end
