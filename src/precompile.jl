function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    precompile(Tuple{typeof(maybe_evaluate_builtin), JuliaStackFrame, Expr})
    precompile(Tuple{typeof(getargs), Vector{Any}, JuliaStackFrame})
    precompile(Tuple{typeof(get_call_framecode), Vector{Any}, JuliaFrameCode, Int})
    for f in (evaluate_call!,
              evaluate_foreigncall!,
              evaluate_methoddef!,
              evaluate_structtype!,
              evaluate_abstracttype!,
              evaluate_primitivetype!)
        precompile(Tuple{typeof(f), Compiled, JuliaStackFrame, Expr, JuliaProgramCounter})
        precompile(Tuple{typeof(f), Vector{JuliaStackFrame}, JuliaStackFrame, Expr, JuliaProgramCounter})
    end
    precompile(Tuple{typeof(lookup_global_refs!), Expr})
    precompile(Tuple{typeof(lookup_or_eval), Compiled, JuliaStackFrame, Any, JuliaProgramCounter})
    precompile(Tuple{typeof(lookup_or_eval), Vector{JuliaStackFrame}, JuliaStackFrame, Any, JuliaProgramCounter})
    precompile(Tuple{typeof(eval_rhs), Compiled, JuliaStackFrame, Expr, JuliaProgramCounter})
    precompile(Tuple{typeof(eval_rhs), Vector{JuliaStackFrame}, JuliaStackFrame, Expr, JuliaProgramCounter})
    precompile(Tuple{typeof(_step_expr!), Compiled, JuliaStackFrame, Any, JuliaProgramCounter, Bool})
    precompile(Tuple{typeof(_step_expr!), Vector{JuliaStackFrame}, JuliaStackFrame, Any, JuliaProgramCounter, Bool})
    for f in (finish!, finish_and_return!)
        precompile(Tuple{typeof(f), Compiled, JuliaStackFrame, JuliaProgramCounter, Bool})
        precompile(Tuple{typeof(f), Vector{JuliaStackFrame}, JuliaStackFrame, JuliaProgramCounter, Bool})
    end
    precompile(Tuple{typeof(prepare_toplevel), Module, Expr})
    precompile(Tuple{typeof(prepare_thunk), Module, Expr})
    precompile(Tuple{typeof(prepare_locals), JuliaFrameCode, Vector{Any}})
    precompile(Tuple{typeof(prepare_args), Any, Vector{Any}, Vector{Any}})
    precompile(Tuple{typeof(prepare_call), Any, Vector{Any}})
    precompile(Tuple{typeof(build_frame), JuliaFrameCode, Vector{Any}, Core.SimpleVector})
    precompile(Tuple{typeof(extract_args), Module, Expr})
    precompile(Tuple{typeof(enter_call), Int, Int})
    precompile(Tuple{typeof(enter_call_expr), Expr})
    precompile(Tuple{typeof(copy_codeinfo), Core.CodeInfo})
    precompile(Tuple{typeof(optimize!), Core.CodeInfo, Module})
    precompile(Tuple{typeof(interpret!), Vector{JuliaStackFrame}, Module, Expr})
    precompile(Tuple{typeof(set_structtype_const), Module, Symbol})
    precompile(Tuple{typeof(namedtuple), Vector{Any}})
    precompile(Tuple{typeof(resolvefc), Any})
    precompile(Tuple{typeof(checkfor_head), typeof(cant_be_lowered), Expr})
    precompile(Tuple{typeof(check_isdefined), JuliaStackFrame, Expr})
    precompile(Tuple{typeof(check_isdefined), JuliaStackFrame, Symbol})
    precompile(Tuple{typeof(find_used), Core.CodeInfo})
    precompile(Tuple{typeof(do_assignment!), JuliaStackFrame, Int, Int})
    precompile(Tuple{typeof(pc_expr), JuliaStackFrame, Nothing})
end
