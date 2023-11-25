function get_toplevel_mi_from_ir(ir, _module::Module)
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.specTypes = Tuple{ir.argtypes...}
    mi.def = _module
    return mi
end

function infer_ir!(ir, interp::Core.Compiler.AbstractInterpreter, mi::Core.Compiler.MethodInstance)
    method_info = Core.Compiler.MethodInfo(#=propagate_inbounds=#true, nothing)
    min_world = world = Core.Compiler.get_world_counter(interp)
    max_world = Base.get_world_counter()
    irsv = Core.Compiler.IRInterpretationState(interp, method_info, ir, mi, ir.argtypes, world, min_world, max_world)
    rt = Core.Compiler._ir_abstract_constant_propagation(interp, irsv)
    return ir
end


# add overloads from Core.Compiler into Base
Base.iterate(compact::Core.Compiler.IncrementalCompact, state) = Core.Compiler.iterate(compact, state)
Base.iterate(compact::Core.Compiler.IncrementalCompact) = Core.Compiler.iterate(compact)
Base.getindex(c::Core.Compiler.IncrementalCompact, args...) = Core.Compiler.getindex(c, args...)
Base.setindex!(c::Core.Compiler.IncrementalCompact, args...) = Core.Compiler.setindex!(c, args...)
Base.setindex!(i::Core.Compiler.Instruction, args...) = Core.Compiler.setindex!(i, args...)


###################################
# Demo

function foo(x)
    a = sin(x+pi/2)
    b = cos(x)
    return a - b
end



input_ir = first(only(Base.code_ircode(foo, Tuple{Float64})))
ir = Core.Compiler.copy(input_ir)
empty!(ir.argtypes)
push!(ir.argtypes, Tuple{})  # the function object itself
push!(ir.argtypes, Float64)  # x
compact = Core.Compiler.IncrementalCompact(ir)
for ((_, idx), inst) in compact
    ssa = Core.SSAValue(idx)
    if Meta.isexpr(inst, :invoke)
        # we can insert nodes, lets print the function objects
        Core.Compiler.insert_node_here!(
            compact,
            Core.Compiler.NewInstruction(
                Expr(:call, println, inst.args[2]),
                Any, # type
                Core.Compiler.NoCallInfo(), # call info
                Int32(1), # line
                Core.Compiler.IR_FLAG_REFINED  # flag
            )
        )
        compact[ssa][:inst] = Expr(:call, inst.args[2:end]...)
        compact[ssa][:type] = Any
        compact[ssa][:flag] |= Core.Compiler.IR_FLAG_REFINED
    end

end
ir = Core.Compiler.finish(compact)
ir = Core.Compiler.compact!(ir)
interp = Core.Compiler.NativeInterpreter()
mi = get_toplevel_mi_from_ir(ir, @__MODULE__);
ir = infer_ir!(ir, interp, mi)
inline_state = Core.Compiler.InliningState(interp)
ir = Core.Compiler.ssa_inlining_pass!(ir, inline_state, #=propagate_inbounds=#true)
ir = Core.Compiler.compact!(ir)
ir = Core.Compiler.sroa_pass!(ir, inline_state)
ir, _ = Core.Compiler.adce_pass!(ir, inline_state)
ir = Core.Compiler.compact!(ir)
f1 = Core.OpaqueClosure(ir; do_compile=true)
f1(1.2)

using JuliaInterpreter, Test
@test @interpret(f1(1.2)) === f1(1.2)
