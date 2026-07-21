function instrument_method(m::Method)
    # Step 1: pick a new name
    name = Symbol(m.name, "#instrumented")
    f = Core.eval(m.module, :(function $name end)) # create the function object (with no methods)
    # Step 2: add an extra argument, `#framedata#`
    # (For simplicity here I'm cheating by assuming no internal locals and no type parameters.
    #  In reality, among other things you'll need to renumber the slots)
    # For help, see https://docs.julialang.org/en/latest/devdocs/ast/#Expr-types-1, section on `method`
    sigm, src = m.sig, copy_codeinfo(Base.uncompressed_ast(m))
    sigt = Core.svec(typeof(f), sigm.parameters[2:end]..., FrameData) # first is #self#
    sigp = Core.svec()
    sigsv = Core.svec(sigt, sigp)
    push!(src.slotnames, Symbol("#framedata#"))
    # Step 3: instrument the body
    #   - for each store to a slotnumber (SlotNumber on the LHS of :(=)), add a
    #     "real" assignment to `#framedata#.locals`
    #   - for each line that is `used`, add a store to `#framedata#.ssavalues`
    # Both of these will require renumbering the ssavalues. Again, for simplicity I'll just cheat:
    # change `x + 1` into `x + 2` for my `inc1` example
    src.code[1].args[end] = 2
    # Step 4: create the new method
    ccall(:jl_method_def, Cvoid, (Any, Any, Any), sigsv, src, m.module)
    # See below about "wrapper methods" that set up `#framedata#` for this method;
    # you might also create the wrapper here?
    return f
end

function typeinf_instrumented(linfo::MethodInstance, params::Core.Compiler.Params)
    # This modifies the source code to add extra instrumentation
    # Step 1: call regular inference. Here, a major goal is to perform inlining,
    # so that we don't have to create so many `framedata`s
    src = Core.Compiler.typeinf_ext(linfo, params)
    # Step 2: replace the `:invoke` Exprs in `isrc` with invokes to `#framedata#`-instrumented variants.
    # You will also have to insert statements to create the framedata for that method.
    # Presumably, the best approach would be to just :invoke a wrapper method that creates a framedata
    # for the method we instrumented via `instrument_method`, and then calls the instrumented method.
    # That wrapper would have the same args as the original function, so it's really just a
    # case of changing which MethodInstance you call.
    # But of course you have to create these MethodInstances for the callees, so this will
    # have to recursively call `instrument_method` followed by `precompile` (which will call
    # this) on all the callees.
    return src
end

function precompile_instrumented(f, atypes)
    # delightfully insane!
    # !!Note!!: before executing this, you need to compile `typeinf_instrumented` and anything it calls
    try
        ccall(:jl_set_typeinf_func, Cvoid, (Any,), typeinf_instrumented)
        precompile(f, atypes)
    finally
        ccall(:jl_set_typeinf_func, Cvoid, (Any,), Core.Compiler.typeinf_ext)
    end
end
