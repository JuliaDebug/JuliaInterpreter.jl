var documenterSearchIndex = {"docs": [

{
    "location": "#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": ""
},

{
    "location": "#JuliaInterpreter-1",
    "page": "Home",
    "title": "JuliaInterpreter",
    "category": "section",
    "text": "This package implements an interpreter for Julia code. Normally, Julia compiles your code when you first execute it; using JuliaInterpreter you can avoid compilation and execute the expressions that define your code directly. Interpreters have a number of applications, including support for stepping debuggers.At a pure user level, there is not much to know:julia> using JuliaInterpreter\n\njulia> a = [1, 2, 5]\n3-element Array{Int64,1}:\n 1\n 2\n 5\n\njulia> sum(a)\n8\n\njulia> @interpret sum(a)\n8Those who want to dive deeper should continue reading."
},

{
    "location": "ast/#",
    "page": "Lowered representation",
    "title": "Lowered representation",
    "category": "page",
    "text": ""
},

{
    "location": "ast/#Lowered-representation-1",
    "page": "Lowered representation",
    "title": "Lowered representation",
    "category": "section",
    "text": "Let\'s start with a demonstration on simple function:function summer(A::AbstractArray{T}) where T\n    s = zero(T)\n    for a in A\n        s += a\n    end\n    return s\nend\n\nA = [1, 2, 5]JuliaInterpreter uses the lowered representation of code:julia> code = @code_lowered summer(A)\nCodeInfo(\n1 ─       s = (Main.zero)($(Expr(:static_parameter, 1)))\n│   %2  = A\n│         #temp# = (Base.iterate)(%2)\n│   %4  = #temp# === nothing\n│   %5  = (Base.not_int)(%4)\n└──       goto #4 if not %5\n2 ┄ %7  = #temp#\n│         a = (Core.getfield)(%7, 1)\n│   %9  = (Core.getfield)(%7, 2)\n│         s = s + a\n│         #temp# = (Base.iterate)(%2, %9)\n│   %12 = #temp# === nothing\n│   %13 = (Base.not_int)(%12)\n└──       goto #4 if not %13\n3 ─       goto #2\n4 ┄       return s\n)To understand this package\'s internals, you need to familiarize yourself with these CodeInfo objects. The numbers on the left correspond to basic blocks; when used in statements these are printed with a hash, e.g., in goto #4 if not %6, the #4 refers to basic block 4. The numbers in the next column–e.g., %1, refer to single static assignment (SSA) values. Each statement (each line of this printout) corresponds to a single SSA value, but only those used later in the code are printed using assignment syntax. Wherever a previous SSA value is used, it\'s referenced by an SSAValue and printed as %6; for example, in goto #4 if not %6, the %6 is the result of evaluating the 6th statement, which is (Base.not_int)(%5), which in turn refers to the result of statement 5. Together lines 5 and 6 correspond to !(#temp# === nothing). (The #temp# means that this was a generated variable name not present explicitly in the original source code.)Before diving into the details, let\'s first look at the statements themselves:julia> code.code\n16-element Array{Any,1}:\n :(_3 = (Main.zero)($(Expr(:static_parameter, 1))))\n :(_2)\n :(_4 = (Base.iterate)(%2))\n :(_4 === nothing)\n :((Base.not_int)(%4))\n :(unless %5 goto %16)\n :(_4)\n :(_5 = (Core.getfield)(%7, 1))\n :((Core.getfield)(%7, 2))\n :(_3 = _3 + _5)\n :(_4 = (Base.iterate)(%2, %9))\n :(_4 === nothing)\n :((Base.not_int)(%12))\n :(unless %13 goto %16)\n :(goto %7)\n :(return _3)You can see directly that the SSA assignments are implicit; they are not directly present in the statement list. The most noteworthy change here is the appearance of objects like _3, which are references that index into local variable slots:julia> code.slotnames\n5-element Array{Any,1}:\n Symbol(\"#self#\")\n :A\n :s\n Symbol(\"#temp#\")\n :aWhen printing the whole CodeInfo object, these slotnames are substituted in. The types of objects that can be in code.code is well-described in the Julia AST documentation."
},

{
    "location": "internals/#",
    "page": "Internals",
    "title": "Internals",
    "category": "page",
    "text": ""
},

{
    "location": "internals/#Internals-1",
    "page": "Internals",
    "title": "Internals",
    "category": "section",
    "text": ""
},

{
    "location": "internals/#Basic-usage-1",
    "page": "Internals",
    "title": "Basic usage",
    "category": "section",
    "text": "The process of executing code in the interpreter is to prepare a frame and then evaluate these statements one-by-one, branching via the goto statements as appropriate. Using the summer example described in Lowered representation, let\'s build a frame:julia> frame = JuliaInterpreter.enter_call(summer, A)\nJuliaStackFrame(JuliaInterpreter.JuliaFrameCode(summer(A::AbstractArray{T,N} where N) where T in Main at REPL[1]:2, CodeInfo(\n1 ─       s = ($(QuoteNode(zero)))($(Expr(:static_parameter, 1)))\n│   %2  = A\n│         #temp# = ($(QuoteNode(iterate)))(%2)\n│   %4  = ($(QuoteNode(===)))(#temp#, nothing)\n│   %5  = ($(QuoteNode(not_int)))(%4)\n└──       goto #4 if not %5\n2 ┄ %7  = #temp#\n│         a = ($(QuoteNode(getfield)))(%7, 1)\n│   %9  = ($(QuoteNode(getfield)))(%7, 2)\n│         s = ($(QuoteNode(+)))(s, a)\n│         #temp# = ($(QuoteNode(iterate)))(%2, %9)\n│   %12 = ($(QuoteNode(===)))(#temp#, nothing)\n│   %13 = ($(QuoteNode(not_int)))(%12)\n└──       goto #4 if not %13\n3 ─       goto #2\n4 ┄       return s\n), Core.TypeMapEntry[#undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef], BitSet([2, 4, 5, 7, 9, 12, 13]), false, false, true), Union{Nothing, Some{Any}}[Some(summer), Some([1, 2, 5]), nothing, nothing, nothing], Any[#undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef], Any[Int64], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])This is a JuliaInterpreter.JuliaStackFrame. The CodeInfo is the most prominent part of this display, and extractable as code = frame.code.code. (It\'s a slightly modified form of one returned by @code_lowered, in that it has been processed by JuliaInterpreter.optimize! to speed up run-time execution.)Much of the rest of the frame holds values needed for or generated by execution. The input arguments are in locals:julia> frame.locals\n5-element Array{Union{Nothing, Some{Any}},1}:\n Some(summer)\n Some([1, 2, 5])\n nothing\n nothing\n nothingThese correspond to the code.slotnames; the first is the #self# argument and the second is the input array. The remaining local variables (e.g., s and a), have not yet been assigned–-we\'ve only built the frame, but we haven\'t yet begun to execute it. The static parameter, T, is stored in frame.sparams:julia> frame.sparams\n1-element Array{Any,1}:\n Int64The Expr(:static_parameter, 1) statement refers to this value.The other main storage is for the generated SSA values:julia> frame.ssavalues\n16-element Array{Any,1}:\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undefSince we haven\'t executed any statements yet, these are all undefined.The other main entity is the so-called program counter, which just indicates the next statement to be executed:julia> frame.pc[]\nJuliaProgramCounter(1)This is stored as a Ref so that it can be updated as execution progresses.Let\'s try executing the first statement. So that we can recurse into calls (e.g., iterate, +, etc.,), we\'ll create a stack of frames and then run the first statement:julia> stack = JuliaInterpreter.JuliaStackFrame[]\n0-element Array{JuliaStackFrame,1}\n\njulia> JuliaInterpreter.step_expr!(stack, frame)\nJuliaProgramCounter(2)This indicates that it ran statement 1 and is prepared to run statement 2. (It\'s worth noting that the first line included a call to zero, so behind the scenes JuliaInterpreter pushed this frame onto stack, created a new frame for zero, executed all the statements, and then popped the stack.) Since the first statement is an assignment of a local variable, let\'s check the locals again:julia> frame.locals\n5-element Array{Union{Nothing, Some{Any}},1}:\n Some(summer)\n Some([1, 2, 5])\n Some(0)\n nothing\n nothingYou can see that the entry corresponding to s has been initialized.The next statement just retrieves one of the slots (the input argument A) and stores it in an SSA value:julia> JuliaInterpreter.step_expr!(stack, frame)\nJuliaProgramCounter(3)\n\njulia> frame.ssavalues\n16-element Array{Any,1}:\n #undef\n    [1, 2, 5]\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undefOne can easily continue this until execution completes, which is indicated when step_expr! returns nothing. Alternatively, use the higher-level JuliaInterpreter.finish!(stack, frame) to step through the entire frame, or JuliaInterpreter.finish_and_return!(stack, frame) to also obtain the return value."
},

{
    "location": "internals/#More-complex-expressions-1",
    "page": "Internals",
    "title": "More complex expressions",
    "category": "section",
    "text": "Sometimes you might have a whole sequence of expressions you want to run. In such cases, your first thought should be prepare_thunk. Here\'s a demonstration:using Test\n\nex = quote\n    x, y = 1, 2\n    @test x + y == 3\nend\n\nframe = JuliaInterpreter.prepare_thunk(Main, ex)\nJuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame)\n\n# output\n\nTest Passed"
},

{
    "location": "internals/#Toplevel-code-and-world-age-1",
    "page": "Internals",
    "title": "Toplevel code and world age",
    "category": "section",
    "text": "Code that defines new structs, new methods, or new modules is a bit more complicated and requires special handling. In such cases, calling finish_and_return! on a frame that defines these new objects and then calls them can trigger a world age error, in which the method is considered to be too new to be run by the currently compiled code. While one can resolve this by using Base.invokelatest, we\'d have to use that strategy throughout the entire package.  This would cause a major reduction in performance. To resolve this issue without leading to performance problems, care is required to return to \"top level\" after defining such objects. This leads to altered syntax for executing such expressions.Here\'s a demonstration of the problem:ex = :(map(x->x^2, [1, 2, 3]))\nframe = JuliaInterpreter.prepare_thunk(Main, ex)\njulia> JuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame)\nERROR: this frame needs to be run a top levelThe reason for this error becomes clearer if we examine frame or look directly at the lowered code:julia> Meta.lower(Main, ex)\n:($(Expr(:thunk, CodeInfo(\n1 ─      $(Expr(:thunk, CodeInfo(\n1 ─     global ##17#18\n│       const ##17#18\n│       $(Expr(:struct_type, Symbol(\"##17#18\"), :((Core.svec)()), :((Core.svec)()), :(Core.Function), :((Core.svec)()), false, 0))\n└──     return\n)))\n│   %2 = (Core.svec)(##17#18, Core.Any)\n│   %3 = (Core.svec)()\n│   %4 = (Core.svec)(%2, %3)\n│        $(Expr(:method, false, :(%4), CodeInfo(quote\n    (Core.apply_type)(Base.Val, 2)\n    (%1)()\n    (Base.literal_pow)(^, x, %2)\n    return %3\nend)))\n│        #17 = %new(##17#18)\n│   %7 = #17\n│   %8 = (Base.vect)(1, 2, 3)\n│   %9 = map(%7, %8)\n└──      return %9\n))))All of the code before the %7 line is devoted to defining the anonymous function x->x^2: it creates a new \"anonymous type\" (here written as ##17#18), and then defines a \"call function\" for this type, equivalent to (##17#18)(x) = x^2.In some cases one can fix this simply by indicating that we want to run this frame at top level:julia> JuliaInterpreter.finish_and_return!(JuliaStackFrame[], frame, true)\n3-element Array{Int64,1}:\n 1\n 4\n 9Here\'s a more fine-grained look at what\'s happening under the hood (and a robust strategy for more complex situations where there may be nested calls of new methods):modexs, _ = JuliaInterpreter.prepare_toplevel(Main, ex)\nstack = JuliaStackFrame[]\nfor (mod, e) in modexs\n    frame = JuliaInterpreter.prepare_thunk(mod, e)\n    while true\n        JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break\n    end\n    JuliaInterpreter.get_return(frame)\nendThis splits the expression into a sequence of frames (here just one, but more complex blocks may be split up into many). Then, each frame is executed until it finishes defining a new method, then returns to top level. The return to top level causes an update in the world age. If the frame hasn\'t been finished yet (if the return value wasn\'t nothing), this continues executing where it left off.(Incidentally, JuliaInterpreter.enter_call(map, x->x^2, [1, 2, 3]) works fine on its own, because the anonymous function is defined by the caller–-you\'ll see that the created frame is very simple.)"
},

{
    "location": "dev_reference/#",
    "page": "Function reference",
    "title": "Function reference",
    "category": "page",
    "text": ""
},

{
    "location": "dev_reference/#Function-reference-1",
    "page": "Function reference",
    "title": "Function reference",
    "category": "section",
    "text": ""
},

{
    "location": "dev_reference/#JuliaInterpreter.@interpret",
    "page": "Function reference",
    "title": "JuliaInterpreter.@interpret",
    "category": "macro",
    "text": "@interpret f(args; kwargs...)\n\nEvaluate f on the specified arguments using the interpreter.\n\nExample\n\njulia> a = [1, 7]\n2-element Array{Int64,1}:\n 1\n 7\n\njulia> sum(a)\n8\n\njulia> @interpret sum(a)\n8\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Top-level-1",
    "page": "Function reference",
    "title": "Top-level",
    "category": "section",
    "text": "@interpret"
},

{
    "location": "dev_reference/#JuliaInterpreter.enter_call",
    "page": "Function reference",
    "title": "JuliaInterpreter.enter_call",
    "category": "function",
    "text": "frame = enter_call(f, args...; kwargs...)\n\nBuild a JuliaStackFrame ready to execute f with the specified positional and keyword arguments.\n\nExample\n\njulia> mymethod(x) = x+1\nmymethod (generic function with 1 method)\n\njulia> JuliaInterpreter.enter_call(mymethod, 1)\nJuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x) in Main at none:1, CodeInfo(\n1 ─ %1 = ($(QuoteNode(+)))(x, 1)\n└──      return %1\n), Core.TypeMapEntry[#undef, #undef], BitSet([1]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some(1)], Any[#undef, #undef], Any[], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])\n\njulia> mymethod(x::Vector{T}) where T = 1\nmymethod (generic function with 2 methods)\n\njulia> JuliaInterpreter.enter_call(mymethod, [1.0, 2.0])\nJuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(\n1 ─     return 1\n), Core.TypeMapEntry[#undef], BitSet([]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some([1.0, 2.0])], Any[#undef], Any[Float64], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])\n\nFor a @generated function you can use enter_call((f, true), args...; kwargs...) to execute the generator of a @generated function, rather than the code that would be created by the generator.\n\nSee enter_call_expr for a similar approach based on expressions.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.enter_call_expr",
    "page": "Function reference",
    "title": "JuliaInterpreter.enter_call_expr",
    "category": "function",
    "text": "frame = enter_call_expr(expr; enter_generated=false)\n\nBuild a JuliaStackFrame ready to execute the expression expr. Set enter_generated=true if you want to execute the generator of a @generated function, rather than the code that would be created by the generator.\n\nExample\n\njulia> mymethod(x) = x+1\nmymethod (generic function with 1 method)\n\njulia> JuliaInterpreter.enter_call_expr(:($mymethod(1)))\nJuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x) in Main at none:1, CodeInfo(\n1 ─ %1 = ($(QuoteNode(+)))(x, 1)\n└──      return %1\n), Core.TypeMapEntry[#undef, #undef], BitSet([1]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some(1)], Any[#undef, #undef], Any[], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])\n\njulia> mymethod(x::Vector{T}) where T = 1\nmymethod (generic function with 2 methods)\n\njulia> a = [1.0, 2.0]\n2-element Array{Float64,1}:\n 1.0\n 2.0\n\njulia> JuliaInterpreter.enter_call_expr(:($mymethod($a)))\nJuliaStackFrame(JuliaInterpreter.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(\n1 ─     return 1\n), Core.TypeMapEntry[#undef], BitSet([]), false, false, true), Union{Nothing, Some{Any}}[Some(mymethod), Some([1.0, 2.0])], Any[#undef], Any[Float64], Int64[], Base.RefValue{Any}(nothing), Base.RefValue{JuliaInterpreter.JuliaProgramCounter}(JuliaProgramCounter(1)), Dict{Symbol,Int64}(), Any[])\n\nSee enter_call for a similar approach not based on expressions.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.build_frame",
    "page": "Function reference",
    "title": "JuliaInterpreter.build_frame",
    "category": "function",
    "text": "frame = build_frame(framecode::JuliaFrameCode, frameargs, lenv)\n\nConstruct a new JuliaStackFrame for framecode, given lowered-code arguments frameargs and static parameters lenv. See JuliaInterpreter.prepare_call for information about how to prepare the inputs.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.determine_method_for_expr",
    "page": "Function reference",
    "title": "JuliaInterpreter.determine_method_for_expr",
    "category": "function",
    "text": "framecode, frameargs, lenv, argtypes = determine_method_for_expr(expr; enter_generated = false)\n\nPrepare all the information needed to execute a particular :call expression expr. For example, try JuliaInterpreter.determine_method_for_expr(:(sum([1,2]))). See JuliaInterpreter.prepare_call for information about the outputs.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.prepare_args",
    "page": "Function reference",
    "title": "JuliaInterpreter.prepare_args",
    "category": "function",
    "text": "frun, allargs = prepare_args(fcall, fargs, kwargs)\n\nPrepare the complete argument sequence for a call to fcall. fargs = [fcall, args...] is a list containing both fcall (the #self# slot in lowered code) and the positional arguments supplied to fcall. kwargs is a list of keyword arguments, supplied either as list of expressions :(kwname=kwval) or pairs :kwname=>kwval.\n\nFor non-keyword methods, frun === fcall, but for methods with keywords frun will be the keyword-sorter function for fcall.\n\nExample\n\njulia> mymethod(x) = 1\nmymethod (generic function with 1 method)\n\njulia> mymethod(x, y; verbose=false) = nothing\nmymethod (generic function with 2 methods)\n\njulia> JuliaInterpreter.prepare_args(mymethod, [mymethod, 15], ())\n(mymethod, Any[mymethod, 15])\n\njulia> JuliaInterpreter.prepare_args(mymethod, [mymethod, 1, 2], [:verbose=>true])\n(getfield( Symbol(\"#kw##mymethod\"))(), Any[#kw##mymethod(), (verbose = true,), mymethod, 1, 2])\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.prepare_call",
    "page": "Function reference",
    "title": "JuliaInterpreter.prepare_call",
    "category": "function",
    "text": "framecode, frameargs, lenv, argtypes = prepare_call(f, allargs; enter_generated=false)\n\nPrepare all the information needed to execute lowered code for f given arguments allargs. f and allargs are the outputs of prepare_args. For @generated methods, set enter_generated=true if you want to extract the lowered code of the generator itself.\n\nOn return framecode is the JuliaFrameCode of the method. frameargs contains the actual arguments needed for executing this frame (for generators, this will be the types of allargs); lenv is the \"environment\", i.e., the static parameters for f given allargs. argtypes is the Tuple-type for this specific call (equivalent to the signature of the MethodInstance).\n\nExample\n\njulia> mymethod(x::Vector{T}) where T = 1\nmymethod (generic function with 1 method)\n\njulia> framecode, frameargs, lenv, argtypes = JuliaInterpreter.prepare_call(mymethod, [mymethod, [1.0,2.0]]);\n\njulia> framecode\nJuliaInterpreter.JuliaFrameCode(mymethod(x::Array{T,1}) where T in Main at none:1, CodeInfo(\n1 ─     return 1\n), Core.TypeMapEntry[#undef], BitSet([]), false, false, true)\n\njulia> frameargs\n2-element Array{Any,1}:\n mymethod\n [1.0, 2.0]\n\njulia> lenv\nsvec(Float64)\n\njulia> argtypes\nTuple{typeof(mymethod),Array{Float64,1}}\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.prepare_thunk",
    "page": "Function reference",
    "title": "JuliaInterpreter.prepare_thunk",
    "category": "function",
    "text": "frame = prepare_thunk(mod::Module, expr::Expr)\n\nPrepare expr for evaluation in mod. expr should be a \"straightforward\" expression, one that does not require special top-level handling (see JuliaInterpreter.prepare_toplevel).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.prepare_toplevel",
    "page": "Function reference",
    "title": "JuliaInterpreter.prepare_toplevel",
    "category": "function",
    "text": "modexs, docexprs = prepare_toplevel(mod::Module, expr::Expr; extract_docexprs=false)\n\nBreak expr into a list modexs of blocks to be successively executed at top level. This is used when expr defines new structs, new methods, or new modules.\n\nmodexs[i] is a (Module, Expr) tuple. A prototype of how to use these is:\n\nstack = JuliaStackFrame[]\nfor modex in modexs\n    frame = JuliaInterpreter.prepare_thunk(modex)\n    while true\n        JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break\n    end\nend\n\nThe while loop here deserves some explanation. Occasionally, a frame may define new methods (e.g., anonymous or local functions) and then call those methods. In such cases, running the entire frame as a single block (e.g., with JuliaInterpreter.finish_and_return! can trigger \"method is too new...\" errors. Instead, this runs each frame, but returns to the caller after any new method is defined. When this loop is running at top level (e.g., in the REPL), this allows the world age to update and thus avoid \"method is too new...\" errors.\n\nPutting the above nested loop inside a function defeats the entire purpose of prepare_toplevel. If necessary, run that loop as\n\nCore.eval(somemodule, Expr(:toplevel, quote\n    body\n))\n\nwhere body executes the loop plus any preparatory statements required to make the necessary variables available at top level in somemodule.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.get_call_framecode",
    "page": "Function reference",
    "title": "JuliaInterpreter.get_call_framecode",
    "category": "function",
    "text": "framecode, lenv = get_call_framecode(fargs, parentframe::JuliaFrameCode, idx::Int)\n\nReturn the framecode and environment for a call specified by fargs = [f, args...] (see prepare_args). parentframecode is the caller, and idx is the program-counter index. If possible, framecode will be looked up from the local method tables of parentframe.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.optimize!",
    "page": "Function reference",
    "title": "JuliaInterpreter.optimize!",
    "category": "function",
    "text": "optimize!(code::CodeInfo, mod::Module)\n\nPerform minor optimizations on the lowered AST in code to reduce execution time of the interpreter. Currently it looks up GlobalRefs (for which it needs mod to know the scope in which this will run) and ensures that no statement includes nested :call expressions (splitting them out into multiple SSA-form statements if needed).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Frame-creation-1",
    "page": "Function reference",
    "title": "Frame creation",
    "category": "section",
    "text": "JuliaInterpreter.enter_call\nJuliaInterpreter.enter_call_expr\nJuliaInterpreter.build_frame\nJuliaInterpreter.determine_method_for_expr\nJuliaInterpreter.prepare_args\nJuliaInterpreter.prepare_call\nJuliaInterpreter.prepare_thunk\nJuliaInterpreter.prepare_toplevel\nJuliaInterpreter.get_call_framecode\nJuliaInterpreter.optimize!"
},

{
    "location": "dev_reference/#JuliaInterpreter.Compiled",
    "page": "Function reference",
    "title": "JuliaInterpreter.Compiled",
    "category": "type",
    "text": "Compiled is a trait indicating that any :call expressions should be evaluated using Julia\'s normal compiled-code evaluation. The alternative is to pass stack=JuliaStackFrame[], which will cause all calls to be evaluated via the interpreter.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.step_expr!",
    "page": "Function reference",
    "title": "JuliaInterpreter.step_expr!",
    "category": "function",
    "text": "pc = step_expr!(stack, frame)\n\nExecute the next statement in frame. pc is the new program counter, or nothing if execution terminates. stack controls call evaluation; stack = Compiled() evaluates :call expressions by normal dispatch, whereas a vector of JuliaStackFrames will use recursive interpretation.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.finish!",
    "page": "Function reference",
    "title": "JuliaInterpreter.finish!",
    "category": "function",
    "text": "pc = finish!(stack, frame, pc=frame.pc[])\n\nRun frame until execution terminates. pc is the program counter for the final statement. stack controls call evaluation; stack = Compiled() evaluates :call expressions by normal dispatch, whereas a vector of JuliaStackFrames will use recursive interpretation.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.finish_and_return!",
    "page": "Function reference",
    "title": "JuliaInterpreter.finish_and_return!",
    "category": "function",
    "text": "ret = finish_and_return!(stack, frame, istoplevel::Bool=false)\nret = finish_and_return!(stack, frame, pc, istoplevel::Bool)\n\nRun frame until execution terminates, and pass back the computed return value. stack controls call evaluation; stack = Compiled() evaluates :call expressions by normal dispatch, whereas a vector of JuliaStackFrames will use recursive interpretation.\n\nOptionally supply the starting pc, if you don\'t want to start at the current location in frame.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.get_return",
    "page": "Function reference",
    "title": "JuliaInterpreter.get_return",
    "category": "function",
    "text": "ret = get_return(frame, pc=frame.pc[])\n\nGet the return value of frame. Throws an error if pc does not point to a return expression. frame must have already been executed so that the return value has been computed (see, e.g., JuliaInterpreter.finish!).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.next_until!",
    "page": "Function reference",
    "title": "JuliaInterpreter.next_until!",
    "category": "function",
    "text": "next_until!(predicate, stack, frame, pc=frame.pc[])\n\nStep through statements of frame until the next statement satifies predicate(stmt).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.through_methoddef_or_done!",
    "page": "Function reference",
    "title": "JuliaInterpreter.through_methoddef_or_done!",
    "category": "function",
    "text": "through_methoddef_or_done!(stack, frame)\n\nRuns frame at top level until it either finishes (e.g., hits a return statement) or defines a new method.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.evaluate_call!",
    "page": "Function reference",
    "title": "JuliaInterpreter.evaluate_call!",
    "category": "function",
    "text": "ret = evaluate_call!(Compiled(), frame::JuliaStackFrame, call_expr, pc)\nret = evaluate_call!(stack,      frame::JuliaStackFrame, call_expr, pc)\n\nEvaluate a :call expression call_expr in the context of frame. The first causes it to be executed using Julia\'s normal dispatch (compiled code), whereas the second recurses in via the interpreter. stack should be a vector of JuliaStackFrame.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.evaluate_foreigncall!",
    "page": "Function reference",
    "title": "JuliaInterpreter.evaluate_foreigncall!",
    "category": "function",
    "text": "ret = evaluate_foreigncall!(stack, frame::JuliaStackFrame, call_expr, pc)\n\nEvaluate a :foreigncall (from a ccall) statement callexpr in the context of frame. stack and pc are unused, but supplied for consistency with evaluate_call!.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.maybe_evaluate_builtin",
    "page": "Function reference",
    "title": "JuliaInterpreter.maybe_evaluate_builtin",
    "category": "function",
    "text": "ret = maybe_evaluate_builtin(frame, call_expr)\n\nIf call_expr is to a builtin function, evaluate it, returning the result inside a Some wrapper. Otherwise, return call_expr.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Frame-execution-1",
    "page": "Function reference",
    "title": "Frame execution",
    "category": "section",
    "text": "JuliaInterpreter.Compiled\nJuliaInterpreter.step_expr!\nJuliaInterpreter.finish!\nJuliaInterpreter.finish_and_return!\nJuliaInterpreter.get_return\nJuliaInterpreter.next_until!\nJuliaInterpreter.through_methoddef_or_done!\nJuliaInterpreter.evaluate_call!\nJuliaInterpreter.evaluate_foreigncall!\nJuliaInterpreter.maybe_evaluate_builtin"
},

{
    "location": "dev_reference/#JuliaInterpreter.JuliaStackFrame",
    "page": "Function reference",
    "title": "JuliaInterpreter.JuliaStackFrame",
    "category": "type",
    "text": "JuliaStackFrame represents the current execution state in a particular call frame.\n\nImportant fields:\n\ncode: the JuliaFrameCode for this frame\nlocals: a vector containing the input arguments and named local variables for this frame. The indexing corresponds to the names in frame.code.code.slotnames.\nssavalues: a vector containing the Static Single Assignment values produced at the current state of execution\nsparams: the static type parameters, e.g., for f(x::Vector{T}) where T this would store the value of T given the particular input x.\npc: the JuliaProgramCounter that typically represents the current position during execution. However, note that some internal functions instead maintain the pc as a local variable, and only update the frame\'s pc when pushing a frame on the stack.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.JuliaFrameCode",
    "page": "Function reference",
    "title": "JuliaInterpreter.JuliaFrameCode",
    "category": "type",
    "text": "JuliaFrameCode holds static information about a method or toplevel code. One JuliaFrameCode can be shared by many JuliaFrameState calling frames.\n\nImportant fields:\n\nscope: the Method or Module in which this frame is to be evaluated\ncode: the CodeInfo object storing (optimized) lowered code\nmethodtables: a vector, each entry potentially stores a \"local method table\" for the corresponding :call expression in code (undefined entries correspond to statements that do not contain :call expressions)\nused: a BitSet storing the list of SSAValues that get referenced by later statements.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.JuliaProgramCounter",
    "page": "Function reference",
    "title": "JuliaInterpreter.JuliaProgramCounter",
    "category": "type",
    "text": "JuliaProgramCounter(next_stmt::Int)\n\nA wrapper specifying the index of the next statement in the lowered code to be executed.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Types-1",
    "page": "Function reference",
    "title": "Types",
    "category": "section",
    "text": "JuliaInterpreter.JuliaStackFrame\nJuliaInterpreter.JuliaFrameCode\nJuliaInterpreter.JuliaProgramCounter"
},

{
    "location": "dev_reference/#JuliaInterpreter.framedict",
    "page": "Function reference",
    "title": "JuliaInterpreter.framedict",
    "category": "constant",
    "text": "framedict[method] returns the JuliaFrameCode for method. For @generated methods, see genframedict.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.genframedict",
    "page": "Function reference",
    "title": "JuliaInterpreter.genframedict",
    "category": "constant",
    "text": "genframedict[(method,argtypes)] returns the JuliaFrameCode for a @generated method method, for the particular argument types argtypes.\n\nThe framecodes stored in genframedict are for the code returned by the generator (i.e, what will run when you call the method on particular argument types); for the generator itself, its framecode would be stored in framedict.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.compiled_methods",
    "page": "Function reference",
    "title": "JuliaInterpreter.compiled_methods",
    "category": "constant",
    "text": "meth ∈ compiled_methods indicates that meth should be run using Compiled rather than recursed into via the interpreter.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Internal-storage-1",
    "page": "Function reference",
    "title": "Internal storage",
    "category": "section",
    "text": "JuliaInterpreter.framedict\nJuliaInterpreter.genframedict\nJuliaInterpreter.compiled_methods"
},

{
    "location": "dev_reference/#JuliaInterpreter.@lookup",
    "page": "Function reference",
    "title": "JuliaInterpreter.@lookup",
    "category": "macro",
    "text": "rhs = @lookup(frame, node)\nrhs = @lookup(mod, frame, node)\n\nThis macro looks up previously-computed values referenced as SSAValues, SlotNumbers, GlobalRefs, QuoteNode, sparam or exception reference expression. It will also lookup symbols in moduleof(frame); this can be supplied ahead-of-time via the 3-argument version. If none of the above apply, the value of node will be returned.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.iswrappercall",
    "page": "Function reference",
    "title": "JuliaInterpreter.iswrappercall",
    "category": "function",
    "text": "Determine whether we are calling a function for which the current function is a wrapper (either because of optional arguments or becaue of keyword arguments).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Utilities-1",
    "page": "Function reference",
    "title": "Utilities",
    "category": "section",
    "text": "JuliaInterpreter.@lookup\nJuliaInterpreter.iswrappercall"
},

]}
