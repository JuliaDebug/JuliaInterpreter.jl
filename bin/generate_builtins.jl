# This file generates builtins.jl.
using InteractiveUtils

const kwinvoke = Core.kwfunc(Core.invoke)

function scopedname(f)
    io = IOBuffer()
    show(io, f)
    fstr = String(take!(io))
    occursin('.', fstr) && return fstr
    tn = typeof(f).name
    Base.isexported(tn.module, Symbol(fstr)) && return fstr
    fsym = Symbol(fstr)
    isdefined(tn.module, fsym) && return string(tn.module) * '.' * fstr
    return "Base." * fstr
end

function nargs(f, table, id)
    # Look up the expected number of arguments in Core.Compiler.tfunc data
    if id !== nothing
        minarg, maxarg, tfunc = table[id]
    else
        minarg = 0
        maxarg = typemax(Int)
    end
    # Specialize arrayref and arrayset for small numbers of arguments
    if f == Core.arrayref
        maxarg = 5
    elseif f == Core.arrayset
        maxarg = 6
    end
    return minarg, maxarg
end

function generate_fcall_nargs(fname, minarg, maxarg)
    # Generate a separate call for each number of arguments
    maxarg < typemax(Int) || error("call this only for constrained number of arguments")
    annotation = fname == "fieldtype" ? "::Type" : ""
    wrapper = "if nargs == "
    for nargs = minarg:maxarg
        wrapper *= "$nargs\n            "
        argcall = ""
        for i = 1:nargs
            argcall *= "@lookup(frame, args[$(i+1)])"
            if i < nargs
                argcall *= ", "
            end
        end
        wrapper *= "return Some{Any}($fname($argcall)$annotation)"
        if nargs < maxarg
            wrapper *= "\n        elseif nargs == "
        end
    end
    wrapper *= "\n        else"
    wrapper *= "\n            return Some{Any}($fname(getargs(args, frame)...)$annotation)"  # to throw the correct error
    wrapper *= "\n        end"
    return wrapper
end

function generate_fcall(f, table, id)
    minarg, maxarg = nargs(f, table, id)
    fname = scopedname(f)
    if maxarg < typemax(Int)
        return generate_fcall_nargs(fname, minarg, maxarg)
    end
    # A built-in with arbitrary or unknown number of arguments.
    # This will (unfortunately) use dynamic dispatch.
    return "return Some{Any}($fname(getargs(args, frame)...))"
end

# `io` is for the generated source file
# `intrinsicsfile` is the path to Julia's `src/intrinsics.h` file
function generate_builtins(file::String)
    open(file, "w") do io
        generate_builtins(io::IO)
    end
end
function generate_builtins(io::IO)
    pat = r"(ADD_I|ALIAS)\((\w*),"
    print(io,
"""
# This file is generated by `generate_builtins.jl`. Do not edit by hand.

function getargs(args, frame)
    nargs = length(args)-1  # skip f
    callargs = resize!(frame.framedata.callargs, nargs)
    for i = 1:nargs
        callargs[i] = @lookup(frame, args[i+1])
    end
    return callargs
end

const kwinvoke = Core.kwfunc(Core.invoke)

function maybe_recurse_expanded_builtin(frame, new_expr)
    f = new_expr.args[1]
    if isa(f, Core.Builtin) || isa(f, Core.IntrinsicFunction)
        return maybe_evaluate_builtin(frame, new_expr, true)
    else
        return new_expr
    end
end

\"\"\"
    ret = maybe_evaluate_builtin(frame, call_expr, expand::Bool)

If `call_expr` is to a builtin function, evaluate it, returning the result inside a `Some` wrapper.
Otherwise, return `call_expr`.

If `expand` is true, `Core._apply_iterate` calls will be resolved as a call to the applied function.
\"\"\"
function maybe_evaluate_builtin(frame, call_expr, expand::Bool)
    args = call_expr.args
    nargs = length(args) - 1
    fex = args[1]
    if isa(fex, QuoteNode)
        f = fex.value
    else
        f = @lookup(frame, fex)
    end
    if !(isa(f, Core.Builtin) || isa(f, Core.IntrinsicFunction))
        return call_expr
    end
    # By having each call appearing statically in the "switch" block below,
    # each gets call-site optimized.
""")
    firstcall = true
    for ft in subtypes(Core.Builtin)
        ft === Core.IntrinsicFunction && continue
        ft === typeof(kwinvoke) && continue  # handle this one later
        head = firstcall ? "if" : "elseif"
        firstcall = false
        f = ft.instance
        fname = scopedname(f)
        # Tuple is common, especially for returned values from calls. It's worth avoiding
        # dynamic dispatch through a call to `ntuple`.
        if f === tuple
            print(io,
"""
    $head f === tuple
        return Some{Any}(ntupleany(i->@lookup(frame, args[i+1]), length(args)-1))
""")
            continue
        elseif f === Core._apply_iterate
            # Resolve varargs calls
            print(io,
"""
    $head f === Core._apply_iterate
        argswrapped = getargs(args, frame)
        if !expand
            return Some{Any}(Core._apply_iterate(argswrapped...))
        end
        aw1 = argswrapped[1]::Function
        @assert aw1 === Core.iterate || aw1 === Core.Compiler.iterate || aw1 === Base.iterate "cannot handle `_apply_iterate` with non iterate as first argument, got \$(aw1), \$(typeof(aw1))"
        new_expr = Expr(:call, argswrapped[2])
        popfirst!(argswrapped) # pop the iterate
        popfirst!(argswrapped) # pop the function
        argsflat = append_any(argswrapped...)
        for x in argsflat
            push!(new_expr.args, QuoteNode(x))
        end
        return maybe_recurse_expanded_builtin(frame, new_expr)
""")
            continue
        elseif f === Core.invoke
            fstr = scopedname(f)
            print(io,
"""
    $head f === $fstr
            argswrapped = getargs(args, frame)
            if !expand
                return Some{Any}($fstr(argswrapped...))
            end
            return Expr(:call, $fstr, argswrapped...)
""")
            continue
        elseif f === Core._call_latest
            print(io,
"""
    elseif f === Core._call_latest
        args = getargs(args, frame)
        if !expand
            return Some{Any}(Core._call_latest(args...))
        end
        new_expr = Expr(:call, args[1])
        popfirst!(args)
        for x in args
            push!(new_expr.args, QuoteNode(x))
        end
        return maybe_recurse_expanded_builtin(frame, new_expr)
""")
        end

        id = findfirst(isequal(f), Core.Compiler.T_FFUNC_KEY)
        fcall = generate_fcall(f, Core.Compiler.T_FFUNC_VAL, id)
        if f in Core.Builtin[
            Core._call_in_world_total, Core.donotdelete,
            Core.get_binding_type, Core.set_binding_type!,
            Core.getglobal, Core.setglobal!,
            Core.modifyfield!, Core.replacefield!, Core.swapfield!,
        ]
            print(io,
"""
    $head @static isdefined($(ft.name.module), $(repr(nameof(f)))) && f === $fname
        $fcall
""")
        else
            print(io,
"""
    $head f === $fname
        $fcall
""")
        end
        firstcall = false
    end
    print(io,
"""
    # Intrinsics
""")
    print(io,
"""
    elseif f === Base.cglobal
        if nargs == 1
            call_expr = copy(call_expr)
            args2 = args[2]
            call_expr.args[2] = isa(args2, QuoteNode) ? args2 : @lookup(frame, args2)
            return Some{Any}(Core.eval(moduleof(frame), call_expr))
        elseif nargs == 2
            call_expr = copy(call_expr)
            args2 = args[2]
            call_expr.args[2] = isa(args2, QuoteNode) ? args2 : @lookup(frame, args2)
            call_expr.args[3] = @lookup(frame, args[3])
            return Some{Any}(Core.eval(moduleof(frame), call_expr))
        end
""")
    # Extract any intrinsics that support varargs
    fva = []
    minmin, maxmax = typemax(Int), 0
    for fsym in names(Core.Intrinsics)
        fsym === :Intrinsics && continue
        isdefined(Base, fsym) || continue
        f = getfield(Base, fsym)
        id = reinterpret(Int32, f) + 1
        minarg, maxarg = nargs(f, Core.Compiler.T_IFUNC, id)
        if maxarg == typemax(Int)
            push!(fva, f)
        else
            minmin = min(minmin, minarg)
            maxmax = max(maxmax, maxarg)
        end
    end
    for f in fva
        id = reinterpret(Int32, f) + 1
        fname = scopedname(f)
        fcall = generate_fcall(f, Core.Compiler.T_IFUNC, id)
        print(io,
"""
    elseif f === $fname
        $fcall
    end
""")
    end
    # Now handle calls with bounded numbers of args
    print(io,
"""
    if isa(f, Core.IntrinsicFunction)
        cargs = getargs(args, frame)
        @static if isdefined(Core.Intrinsics, :have_fma)
            if f === Core.Intrinsics.have_fma && length(cargs) == 1
                cargs1 = cargs[1]
                if cargs1 == Float64
                    return Some{Any}(FMA_FLOAT64[])
                elseif cargs1 == Float32
                    return Some{Any}(FMA_FLOAT32[])
                elseif cargs1 == Float16
                    return Some{Any}(FMA_FLOAT16[])
                end
            end
        end
        if f === Core.Intrinsics.muladd_float && length(cargs) == 3
            a, b, c = cargs
            Ta, Tb, Tc = typeof(a), typeof(b), typeof(c)
            if !(Ta == Tb == Tc)
                error("muladd_float: types of a, b, and c must match")
            end
            if Ta == Float64 && FMA_FLOAT64[]
                f = Core.Intrinsics.fma_float
            elseif Ta == Float32 && FMA_FLOAT32[]
                f = Core.Intrinsics.fma_float
            elseif Ta == Float16 && FMA_FLOAT16[]
                f = Core.Intrinsics.fma_float
            end
        end
        return Some{Any}(ccall(:jl_f_intrinsic_call, Any, (Any, Ptr{Any}, UInt32), f, cargs, length(cargs)))
""")
    print(io,
"""
    end
    if isa(f, typeof(kwinvoke))
        return Some{Any}(kwinvoke(getargs(args, frame)...))
    end
    return call_expr
end
""")
end

generate_builtins(joinpath(@__DIR__, "..", "src", "builtins.jl"))
