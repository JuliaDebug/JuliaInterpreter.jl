const __bodyfunction__ = Dict{Method,Any}()

# Find keyword "body functions" (the function that contains the body
# as written by the developer, called after all missing keyword-arguments
# have been assigned values), in a manner that doesn't depend on
# gensymmed names. We'd like to use LoweredCodeUtils, but since it
# depends on this package we can't do that.
# `mnokw` is the method that gets called when you invoke it without
# supplying any keywords.
function __lookup_kwbody__(mnokw::Method)
    function getsym(arg)
        isa(arg, Symbol) && return arg
        @assert isa(arg, GlobalRef)
        return arg.name
    end

    f = get(__bodyfunction__, mnokw, nothing)
    if f === nothing
        fmod = mnokw.module
        # The lowered code for `mnokw` should look like
        #   %1 = mkw(kwvalues..., #self#, args...)
        #        return %1
        # where `mkw` is the name of the "active" keyword body-function.
        ast = Base.uncompressed_ast(mnokw)
        if isa(ast, CodeInfo) && length(ast.code) >= 2
            callexpr = ast.code[end-1]
            if isa(callexpr, Expr) && callexpr.head == :call
                fsym = callexpr.args[1]
                if isa(fsym, Symbol)
                    f = getfield(fmod, fsym)
                elseif isa(fsym, GlobalRef)
                    if fsym.mod === Core && fsym.name === :_apply
                        f = getfield(mnokw.module, getsym(callexpr.args[2]))
                    elseif fsym.mod === Core && fsym.name === :_apply_iterate
                        f = getfield(mnokw.module, getsym(callexpr.args[3]))
                    else
                        f = getfield(fsym.mod, fsym.name)
                    end
                else
                    f = missing
                end
            else
                f = missing
            end
        else
            f = missing
        end
        __bodyfunction__[mnokw] = f
    end
    return f
end

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    # Ones not marked `@assert` fail on older Julia versions

    precompile(Tuple{typeof(append_any),Any})
    @assert precompile(Tuple{typeof(breakpoint),String,Int})
    @assert precompile(Tuple{typeof(breakpoint),Function})
    @assert precompile(Tuple{typeof(build_compiled_call!),Expr,Function,Core.CodeInfo,Int,Int,Array{Symbol,1},Module})
    @assert precompile(Tuple{typeof(build_compiled_call!), Expr, Symbol, Core.CodeInfo, Int, Int, Vector{Symbol}, Module})
    @assert precompile(Tuple{typeof(check_isdefined), Frame, Any})
    @assert precompile(Tuple{typeof(copy_codeinfo), Core.CodeInfo})
    @assert precompile(Tuple{typeof(debug_command),Frame,Symbol,Bool})
    @assert precompile(Tuple{typeof(debug_command),Frame,Symbol})
    let fbody = try __lookup_kwbody__(which(debug_command, (Any,Frame,Symbol,Bool,))) catch missing end
        if !ismissing(fbody)
            @assert precompile(fbody, (Nothing,typeof(debug_command),Any,Frame,Symbol,Bool,))
        end
    end
    @assert precompile(Tuple{Core.kwftype(typeof(debug_command)),NamedTuple{(:line,),Tuple{Int}},typeof(debug_command),Frame,Symbol})
    @assert precompile(Tuple{typeof(disable),BreakpointSignature})
    @assert precompile(Tuple{typeof(do_assignment!), Frame, Any, Any})
    @assert precompile(Tuple{typeof(enter_call), Int, Int})
    @assert precompile(Tuple{typeof(enter_call_expr), Expr})
    @assert precompile(Tuple{typeof(enter_call),Any,Any})
    @assert precompile(Tuple{typeof(enter_call_expr),Expr})
    precompile(Tuple{Core.kwftype(typeof(enter_call)),NamedTuple{(:b,),Tuple{Int}},typeof(enter_call),Any,Any})
    precompile(Tuple{Core.kwftype(typeof(enter_call)),NamedTuple{(:rev,),Tuple{Bool}},typeof(enter_call),Any,Any})
    precompile(Tuple{Core.kwftype(typeof(enter_call)),NamedTuple{(:x, :y),Tuple{Int,Int}},typeof(enter_call),Any,Any,Vararg{Any,N} where N})
    @assert precompile(evaluate_call_recurse!, (Function, Frame, Expr))
    @assert precompile(evaluate_call_compiled!, (Compiled, Frame, Expr))
    @assert precompile(Tuple{typeof(eval_code),Frame,Expr})
    @assert precompile(Tuple{typeof(eval_code),Frame,String})
    for f in (evaluate_structtype,
              evaluate_abstracttype,
              evaluate_primitivetype)
        @assert precompile(Tuple{typeof(f), Any, Frame, Expr})
    end
    @assert precompile(Tuple{typeof(eval_rhs), Any, Frame, Expr})
    @assert precompile(Tuple{typeof(evaluate_foreigncall), Frame, Expr})
    @assert precompile(Tuple{typeof(evaluate_methoddef), Frame, Expr})
    @assert precompile(Tuple{typeof(extract_args), Module, Expr})
    @assert precompile(Tuple{typeof(find_used), Core.CodeInfo})
    @assert precompile(Tuple{typeof(finish!),Any,Frame,Bool})
    @assert precompile(Tuple{typeof(firehooks),Function,BreakpointFileLocation})
    @assert precompile(Tuple{typeof(firehooks),Function,BreakpointSignature})
    @assert precompile(Tuple{typeof(getargs), Vector{Any}, Frame})
    @assert precompile(Tuple{typeof(get_call_framecode), Vector{Any}, FrameCode, Int})
    @assert precompile(Tuple{typeof(lookup_global_refs!), Expr})
    @assert precompile(Tuple{typeof(lookup_or_eval), Any, Frame, Any})
    @assert precompile(Tuple{typeof(maybe_evaluate_builtin), Frame, Expr, Bool})
    @assert precompile(Tuple{typeof(maybe_step_through_wrapper!),Frame})
    @assert precompile(Tuple{typeof(finish_and_return!),Frame})
    @assert precompile(Tuple{typeof(handle_err),Any,Frame,MethodError})
    @assert precompile(Tuple{typeof(namedtuple), Vector{Any}})
    @assert precompile(Tuple{typeof(optimize!), Core.CodeInfo, Module})
    @assert precompile(Tuple{typeof(optimize!), Core.CodeInfo, Method})
    @assert precompile(Tuple{typeof(pc_expr), Frame})
    @assert precompile(Tuple{typeof(prepare_args), Any, Vector{Any}, Vector{Any}})
    @assert precompile(Tuple{typeof(prepare_call), Any, Vector{Any}})
    @assert precompile(Tuple{typeof(Core.kwfunc(prepare_call)), NamedTuple{(:enter_generated,),Tuple{Bool}}, typeof(prepare_call), Function, Vector{Any}})
    @assert precompile(Tuple{typeof(prepare_frame), FrameCode, Vector{Any}, Core.SimpleVector})
    precompile(Tuple{typeof(Core.kwfunc(prepare_framecode)), NamedTuple{(:enter_generated,),Tuple{Bool}}, typeof(prepare_framecode), Method, Any})
    @assert precompile(Tuple{typeof(prepare_framedata), FrameCode, Vector{Any}, SimpleVector, Bool})
    @assert precompile(Tuple{typeof(prepare_thunk), Module, Expr})
    @assert precompile(Tuple{typeof(prepare_thunk), Module, Expr, Bool})
    @assert precompile(Tuple{typeof(renumber_ssa!), Vector{Any}, Vector{Int}})
    @assert precompile(Tuple{typeof(repr),BreakpointRef})
    @assert precompile(Tuple{typeof(resolvefc), Frame, Any})
    @assert precompile(Tuple{typeof(set_structtype_const), Module, Symbol})
    @assert precompile(Tuple{typeof(show),Base.GenericIOBuffer{Array{UInt8,1}},Frame})
    @assert precompile(Tuple{typeof(split_expressions), Module, Expr})
    @assert precompile(Tuple{typeof(split_expressions!), Vector{Tuple{Module,Expr}}, Dict{Module,Vector{Expr}}, Expr, Module, Expr})
    @assert precompile(Tuple{typeof(Core.kwfunc(split_expressions)), NamedTuple{(:extract_docexprs,),Tuple{Bool}}, typeof(split_expressions), Module, Expr})
    let fbody = try __lookup_kwbody__(which(split_expressions!, (Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Expr,Module,Expr,))) catch missing end
        if !ismissing(fbody)
            @assert precompile(fbody, (Bool,Bool,Base.Iterators.Pairs{Symbol,Bool,Tuple{Symbol},NamedTuple{(:istoplevel,),Tuple{Bool}}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Expr,Module,Expr,))
        end
    end
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions!)),NamedTuple{(:filename, :extract_docexprs),Tuple{String,Bool}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions!)),NamedTuple{(:filename, :extract_docexprs),Tuple{Symbol,Bool}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions!)),NamedTuple{(:filename, :extract_docexprs, :eval, :istoplevel),Tuple{String,Bool,Bool,Bool}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Expr,Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions!)),NamedTuple{(:filename, :extract_docexprs, :eval, :istoplevel),Tuple{Symbol,Bool,Bool,Bool}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Expr,Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions!)),NamedTuple{(:filename, :istoplevel, :eval),Tuple{Symbol,Bool,Bool}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions!)),NamedTuple{(:filename,),Tuple{String}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions!)),NamedTuple{(:filename,),Tuple{Symbol}},typeof(split_expressions!),Vector{Tuple{Module,Expr}},Dict{Module,Vector{Expr}},Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions)),NamedTuple{(:extract_docexprs,),Tuple{Bool}},typeof(split_expressions),Module,Expr})
    @assert precompile(Tuple{Core.kwftype(typeof(split_expressions)),NamedTuple{(:istoplevel, :eval),Tuple{Bool,Bool}},typeof(split_expressions),Module,Expr})
    @assert precompile(Tuple{typeof(step_expr!), Any, Frame, Any, Bool})

    for f in (finish!, finish_and_return!, finish_stack!, next_call!, maybe_next_call!, next_line!)
        @assert precompile(Tuple{typeof(f), Any, Frame, Bool})
    end
    @assert precompile(Tuple{typeof(through_methoddef_or_done!), Any, Frame})
end
