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
    "text": "This package implements an interpreter for Julia code. Normally, Julia compiles your code when you first execute it; using JuliaInterpreter you can avoid compilation and execute the expressions that define your code directly. Interpreters have a number of applications, including support for stepping debuggers."
},

{
    "location": "#Use-an-an-interpreter-1",
    "page": "Home",
    "title": "Use an an interpreter",
    "category": "section",
    "text": "Using this package as an interpreter is straightforward:julia> using JuliaInterpreter\n\njulia> list = [1, 2, 5]\n3-element Array{Int64,1}:\n 1\n 2\n 5\n\njulia> sum(list)\n8\n\njulia> @interpret sum(list)\n8"
},

{
    "location": "#Breakpoints-1",
    "page": "Home",
    "title": "Breakpoints",
    "category": "section",
    "text": "You can interrupt execution by setting breakpoints. You can set breakpoints via packages that explicitly target debugging, like Juno, Debugger, and Rebugger. But all of these just leverage the core functionality defined in JuliaInterpreter, so here we\'ll illustrate it without using any of these other packages.Let\'s set a conditional breakpoint, to be triggered any time one of the elements in the argument to sum is bigger than 4:julia> bp = @breakpoint sum([1, 2]) any(x->x>4, a);Note that in writing the condition, we used a, the name of the argument to the relevant method of sum. Conditionals should be written using a combination of argument and parameter names of the method into which you\'re inserting a breakpoint; you can also use any globally-available name (as used here with the any function).Now let\'s see what happens:julia> @interpret sum([1,2,3])  # no element bigger than 4, breakpoint should not trigger\n6\n\njulia> frame, bpref = @interpret sum([1,2,5])  # should trigger breakpoint\n(Frame for sum(a::AbstractArray) in Base at reducedim.jl:648\nc 1* 648  1 ─      nothing\n  2  648  │   %2 = (Base.#sum#550)(Colon(), #self#, a)\n  3  648  └──      return %2\na = [1, 2, 5], breakpoint(sum(a::AbstractArray) in Base at reducedim.jl:648, line 648))frame is described in more detail on the next page; for now, suffice it to say that the c in the leftmost column indicates the presence of a conditional breakpoint upon entry to sum. bpref is a reference to the breakpoint of type BreakpointRef. The breakpoint bp we created can be manipulated at the command linejulia> disable(bp)\n\njulia> @interpret sum([1,2,5])\n8\n\njulia> enable(bp)\n\njulia> @interpret sum([1,2,5])\n(Frame for sum(a::AbstractArray) in Base at reducedim.jl:648\nc 1* 648  1 ─      nothing\n  2  648  │   %2 = (Base.#sum#550)(Colon(), #self#, a)\n  3  648  └──      return %2\na = [1, 2, 5], breakpoint(sum(a::AbstractArray) in Base at reducedim.jl:648, line 648))disable and enable allow you to turn breakpoints off and on without losing any conditional statements you may have provided; remove allows a permanent removal of the breakpoint. You can use remove() to remove all breakpoints in all methods.@breakpoint allows you to optionally specify a line number at which the breakpoint is to be set. You can also use a functional form, breakpoint, to specify file/line combinations or that you want to break on entry to any method of a particular function. At present, note that some of this functionality requires that you be running Revise.jl.It is, in addition, possible to halt execution when otherwise an error would be thrown. This functionality is enabled using break_on and disabled with break_off:julia> function f_outer()\n           println(\"before error\")\n           f_inner()\n           println(\"after error\")\n       end;\n\njulia> f_inner() = error(\"inner error\");\n\njulia> break_on(:error)\n\njulia> fr, pc = @interpret f_outer()\nbefore error\n(Frame for f_outer() in Main at none:2\n  1  2  1 ─      (println)(\"before error\")\n  2* 3  │        (f_inner)()\n  3  4  │   %3 = (println)(\"after error\")\n  4  4  └──      return %3\ncallee: f_inner() in Main at none:1, breakpoint(error(s::AbstractString) in Base at error.jl:33, line 33, ErrorException(\"inner error\")))\n\njulia> leaf(fr)\nFrame for error(s::AbstractString) in Base at error.jl:33\n  1  33  1 ─ %1 = (ErrorException)(s)\n  2* 33  │   %2 = (throw)(%1)\n  3  33  └──      return %2\ns = \"inner error\"\ncaller: f_inner() in Main at none:1\n\njulia> typeof(pc)\nBreakpointRef\n\njulia> pc.err\nErrorException(\"inner error\")\n\njulia> break_off(:error)\n\njulia> @interpret f_outer()\nbefore error\nERROR: inner error\nStacktrace:\n[...]Finally, you can set breakpoints using @bp:julia> function myfunction(x, y)\n           a = 1\n           b = 2\n           x > 3 && @bp\n           return a + b + x + y\n       end\nmyfunction (generic function with 1 method)\n\njulia> @interpret myfunction(1, 2)\n6\n\njulia> @interpret myfunction(5, 6)\n(Frame for myfunction(x, y) in Main at none:2\n⋮\n  3  4  │   %3 = (>)(x, 3)\n  4  4  └──      goto #3 if not %3\nb 5* 4  2 ─      nothing\n  6  4  └──      goto #3\n  7  5  3 ┄ %7 = (+)(a, b, x, y)\n⋮\nx = 5\ny = 6\na = 1\nb = 2, breakpoint(myfunction(x, y) in Main at none:2, line 4))Here the breakpoint is marked with a b indicating that it is an unconditional breakpoint. Because we placed it inside the condition x > 3, we\'ve achieved a conditional outcome.When using @bp in source-code files, the use of Revise is recommended, since it allows you to add breakpoints, test code, and then remove the breakpoints from the code without restarting Julia."
},

{
    "location": "#debug_command-1",
    "page": "Home",
    "title": "debug_command",
    "category": "section",
    "text": "You can control execution of frames via debug_command. Authors of debugging applications should target debug_command for their interaction with JuliaInterpreter."
},

{
    "location": "ast/#",
    "page": "Lowered representation",
    "title": "Lowered representation",
    "category": "page",
    "text": "note: Note\nThis page and the next are designed to teach a little more about the internals. Depending on your interest, you may be able to skip them."
},

{
    "location": "ast/#Lowered-representation-1",
    "page": "Lowered representation",
    "title": "Lowered representation",
    "category": "section",
    "text": "JuliaInterpreter uses the lowered representation of code. The key advantage of lowered representation is that it is fairly well circumscribed:There are only a limited number of legal statements that can appear in lowered code\nEach statement is \"unpacked\" to essentially do one thing\nScoping of variables is simplified via the slot mechanism, described below\nNames are fully resolved by module\nMacros are expandedJulia AST describes the kinds of objects that can appear in lowered code.Let\'s start with a demonstration on a simple function:function summer(A::AbstractArray{T}) where T\n    s = zero(T)\n    for a in A\n        s += a\n    end\n    return s\nend\n\nA = [1, 2, 5]To interpret lowered representation, it maybe be useful to rewrite the body of summer in the following ways. First let\'s use an intermediate representation that expands the for a in A ... end loop:    s = zero(T)\n    temp = iterate(A)         # `for` loops get lowered to `iterate/while` loops\n    while temp !== nothing\n        a, state = temp\n        s += a\n        temp = iterate(A, state)\n    end\n    return sThe lowered code takes the additional step of resolving the names by module and turning all the branching into @goto/@label equivalents:    # Code starting at line 2 (the first line of the body)\n    s = Main.zero(T)       # T corresponds to the first parameter, i.e., $(Expr(:static_parameter, 1))\n\n    # Code starting at line 3\n    temp = Base.iterate(A) # here temp = @_4\n    if temp === nothing    # this comparison gets stored as %4, and %5 stores !(temp===nothing)\n        @goto block4\n    end\n\n    @label block2\n        ## BEGIN block2\n        a, state = temp[1], temp[2]  # these correspond to the `getfield` calls, state is %9\n\n        # Code starting at line 4\n        s = s + a\n\n        # Code starting at line 5\n        temp = iterate(A, state)     # A is also %2\n        if temp === nothing\n            @goto block4             # the `while` condition was false\n        end\n        ## END block2\n\n    @goto block2           # here the `while` condition is still true\n\n    # Code starting at line 6\n    @label block4\n        ## BEGIN block4\n        return s\n        ## END block4This has very close correspondence to the lowered representation:julia> code = @code_lowered debuginfo=:source summer(A)\nCodeInfo(\n    @ REPL[1]:2 within `summer\'\n1 ─       s = Main.zero($(Expr(:static_parameter, 1)))\n│   @ REPL[1]:3 within `summer\'\n│   %2  = A\n│         @_4 = Base.iterate(%2)\n│   %4  = @_4 === nothing\n│   %5  = Base.not_int(%4)\n└──       goto #4 if not %5\n2 ┄ %7  = @_4\n│         a = Core.getfield(%7, 1)\n│   %9  = Core.getfield(%7, 2)\n│   @ REPL[1]:4 within `summer\'\n│         s = s + a\n│         @_4 = Base.iterate(%2, %9)\n│   %12 = @_4 === nothing\n│   %13 = Base.not_int(%12)\n└──       goto #4 if not %13\n3 ─       goto #2\n    @ REPL[1]:6 within `summer\'\n4 ┄       return s\n)note: Note\nNot all Julia versions support debuginfo. If the command above fails for you, just omit the debuginfo=:source portion.To understand this package\'s internals, you need to familiarize yourself with these CodeInfo objects. The lines that start with @ REPL[1]:n indicate the source line of the succeeding block of statements; here we defined this method in the REPL, so the source file is REPL[1]; the number after the colon is the line number.The numbers on the left correspond to basic blocks, as we annotated with @label block2 above. When used in statements these are printed with a hash, e.g., in goto #4 if not %5, the #4 refers to basic block 4. The numbers in the next column–e.g., %2, refer to single static assignment (SSA) values. Each statement (each line of this printout) corresponds to a single SSA value, but only those used later in the code are printed using assignment syntax. Wherever a previous SSA value is used, it\'s referenced by an SSAValue and printed as %5; for example, in goto #4 if not %5, the %5 is the result of evaluating the 5th statement, which is (Base.not_int)(%4), which in turn refers to the result of statement 4. Finally, temporary variables here are shown as @_4; the _ indicates a slot, either one of the input arguments or a local variable, and the 4 means the 4th one. Together lines 4 and 5 correspond to !(@_4 === nothing), where @_4 has been assigned the result of the call to iterate occurring on line 3. (In some Julia versions, this may be printed as #temp#, similar to how we named it in our alternative implementation above.)Let\'s look at a couple of the fields of the CodeInfo. First, the statements themselves:julia> code.code\n16-element Array{Any,1}:\n :(_3 = Main.zero($(Expr(:static_parameter, 1))))\n :(_2)\n :(_4 = Base.iterate(%2))\n :(_4 === nothing)\n :(Base.not_int(%4))\n :(unless %5 goto %16)\n :(_4)\n :(_5 = Core.getfield(%7, 1))\n :(Core.getfield(%7, 2))\n :(_3 = _3 + _5)\n :(_4 = Base.iterate(%2, %9))\n :(_4 === nothing)\n :(Base.not_int(%12))\n :(unless %13 goto %16)\n :(goto %7)\n :(return _3)You can see directly that the SSA assignments are implicit; they are not directly present in the statement list. The most noteworthy change here is the appearance of more objects like _3, which are references that index into local variable slots:julia> code.slotnames\n5-element Array{Any,1}:\n Symbol(\"#self#\")\n :A\n :s\n Symbol(\"\")\n :aWhen printing the whole CodeInfo object, these slotnames are substituted in (unless they are empty, as was the case for @_4 above)."
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
    "text": "The process of executing code in the interpreter is to prepare a frame and then evaluate these statements one-by-one, branching via the goto statements as appropriate. Using the summer example described in Lowered representation, let\'s build a frame:julia> frame = JuliaInterpreter.enter_call(summer, A)\nFrame for summer(A::AbstractArray{T,N} where N) where T in Main at REPL[2]:2\n   1* 2  1 ─       s = (zero)($(Expr(:static_parameter, 1)))\n   2  3  │   %2  = A\n   3  3  │         #temp# = (iterate)(%2)\n⋮\nA = [1, 2, 5]\nT = Int64This is a Frame. Only a portion of the CodeInfo is shown, a small region surrounding the current statement (marked with * or in yellow text). The full CodeInfo can be extracted as code = frame.framecode.src. (It\'s a slightly modified form of one returned by @code_lowered, in that it has been processed by JuliaInterpreter.optimize! to speed up run-time execution.)frame has another field, framedata, that holds values needed for or generated by execution. The input arguments and local variables are in locals:julia> frame.framedata.locals\n5-element Array{Union{Nothing, Some{Any}},1}:\n Some(summer)\n Some([1, 2, 5])\n nothing\n nothing\n nothingThese correspond to the code.slotnames; the first is the #self# argument and the second is the input array. The remaining local variables (e.g., s and a), have not yet been assigned–-we\'ve only built the frame, but we haven\'t yet begun to execute it. The static parameter, T, is stored in frame.framedata.sparams:julia> frame.framedata.sparams\n1-element Array{Any,1}:\n Int64The Expr(:static_parameter, 1) statement refers to this value.The other main storage is for the generated SSA values:julia> frame.framedata.ssavalues\n16-element Array{Any,1}:\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undefSince we haven\'t executed any statements yet, these are all undefined.The other main entity is the so-called program counter, which just indicates the next statement to be executed:julia> frame.pc\n1Let\'s try executing the first statement:julia> JuliaInterpreter.step_expr!(frame)\n2This indicates that it ran statement 1 and is prepared to run statement 2. (It\'s worth noting that the first line included a call to zero, so behind the scenes JuliaInterpreter created a new frame for zero, executed all the statements, and then popped back to frame.) Since the first statement is an assignment of a local variable, let\'s check the locals again:julia> frame.framedata.locals\n5-element Array{Union{Nothing, Some{Any}},1}:\n Some(summer)\n Some([1, 2, 5])\n Some(0)\n nothing\n nothingYou can see that the entry corresponding to s has been initialized.The next statement just retrieves one of the slots (the input argument A) and stores it in an SSA value:julia> JuliaInterpreter.step_expr!(frame)\n3\n\njulia> frame.framedata.ssavalues\n16-element Array{Any,1}:\n #undef\n    [1, 2, 5]\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undef\n #undefOne can easily continue this until execution completes, which is indicated when step_expr! returns nothing. Alternatively, use the higher-level JuliaInterpreter.finish!(frame) to step through the entire frame, or JuliaInterpreter.finish_and_return!(frame) to also obtain the return value."
},

{
    "location": "internals/#More-complex-expressions-1",
    "page": "Internals",
    "title": "More complex expressions",
    "category": "section",
    "text": "Sometimes you might have a whole sequence of expressions you want to run. In such cases, your first thought should be prepare_thunk. Here\'s a demonstration:using Test\n\nex = quote\n    x, y = 1, 2\n    @test x + y == 3\nend\n\nframe = JuliaInterpreter.prepare_thunk(Main, ex)\nJuliaInterpreter.finish_and_return!(frame)\n\n# output\n\nTest Passed"
},

{
    "location": "internals/#Toplevel-code-and-world-age-1",
    "page": "Internals",
    "title": "Toplevel code and world age",
    "category": "section",
    "text": "Code that defines new structs, new methods, or new modules is a bit more complicated and requires special handling. In such cases, calling finish_and_return! on a frame that defines these new objects and then calls them can trigger a world age error, in which the method is considered to be too new to be run by the currently compiled code. While one can resolve this by using Base.invokelatest, we\'d have to use that strategy throughout the entire package.  This would cause a major reduction in performance. To resolve this issue without leading to performance problems, care is required to return to \"top level\" after defining such objects. This leads to altered syntax for executing such expressions.Here\'s a demonstration of the problem:ex = :(map(x->x^2, [1, 2, 3]))\nframe = JuliaInterpreter.prepare_thunk(Main, ex)\njulia> JuliaInterpreter.finish_and_return!(frame)\nERROR: this frame needs to be run a top levelThe reason for this error becomes clearer if we examine frame or look directly at the lowered code:julia> Meta.lower(Main, ex)\n:($(Expr(:thunk, CodeInfo(\n1 ─      $(Expr(:thunk, CodeInfo(\n1 ─     global ##17#18\n│       const ##17#18\n│       $(Expr(:struct_type, Symbol(\"##17#18\"), :((Core.svec)()), :((Core.svec)()), :(Core.Function), :((Core.svec)()), false, 0))\n└──     return\n)))\n│   %2 = (Core.svec)(##17#18, Core.Any)\n│   %3 = (Core.svec)()\n│   %4 = (Core.svec)(%2, %3)\n│        $(Expr(:method, false, :(%4), CodeInfo(quote\n    (Core.apply_type)(Base.Val, 2)\n    (%1)()\n    (Base.literal_pow)(^, x, %2)\n    return %3\nend)))\n│        #17 = %new(##17#18)\n│   %7 = #17\n│   %8 = (Base.vect)(1, 2, 3)\n│   %9 = map(%7, %8)\n└──      return %9\n))))All of the code before the %7 line is devoted to defining the anonymous function x->x^2: it creates a new \"anonymous type\" (here written as ##17#18), and then defines a \"call function\" for this type, equivalent to (##17#18)(x) = x^2.In some cases one can fix this simply by indicating that we want to run this frame at top level:julia> JuliaInterpreter.finish_and_return!(frame, true)\n3-element Array{Int64,1}:\n 1\n 4\n 9Here\'s a more fine-grained look at what\'s happening under the hood (and a robust strategy for more complex situations where there may be nested calls of new methods):modexs, _ = JuliaInterpreter.split_expressions(Main, ex)\nfor (mod, e) in modexs\n    frame = JuliaInterpreter.prepare_thunk(mod, e)\n    while true\n        JuliaInterpreter.through_methoddef_or_done!(frame) === nothing && break\n    end\n    JuliaInterpreter.get_return(frame)\nendThis splits the expression into a sequence of frames (here just one, but more complex blocks may be split up into many). Then, each frame is executed until it finishes defining a new method, then returns to top level. The return to top level causes an update in the world age. If the frame hasn\'t been finished yet (if the return value wasn\'t nothing), this continues executing where it left off.(Incidentally, JuliaInterpreter.enter_call(map, x->x^2, [1, 2, 3]) works fine on its own, because the anonymous function is defined by the caller–-you\'ll see that the created frame is very simple.)"
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
    "location": "dev_reference/#Running-the-interpreter-1",
    "page": "Function reference",
    "title": "Running the interpreter",
    "category": "section",
    "text": "@interpret"
},

{
    "location": "dev_reference/#JuliaInterpreter.enter_call",
    "page": "Function reference",
    "title": "JuliaInterpreter.enter_call",
    "category": "function",
    "text": "frame = enter_call(f, args...; kwargs...)\n\nBuild a Frame ready to execute f with the specified positional and keyword arguments.\n\nExample\n\njulia> mymethod(x) = x+1\nmymethod (generic function with 1 method)\n\njulia> JuliaInterpreter.enter_call(mymethod, 1)\nFrame for mymethod(x) in Main at none:1\n  1* 1  1 ─ %1 = (+)(x, 1)\n  2  1  └──      return %1\nx = 1\n\njulia> mymethod(x::Vector{T}) where T = 1\nmymethod (generic function with 2 methods)\n\njulia> JuliaInterpreter.enter_call(mymethod, [1.0, 2.0])\nFrame for mymethod(x::Array{T,1}) where T in Main at none:1\n  1* 1  1 ─     return 1\nx = [1.0, 2.0]\nT = Float64\n\nFor a @generated function you can use enter_call((f, true), args...; kwargs...) to execute the generator of a @generated function, rather than the code that would be created by the generator.\n\nSee enter_call_expr for a similar approach based on expressions.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.enter_call_expr",
    "page": "Function reference",
    "title": "JuliaInterpreter.enter_call_expr",
    "category": "function",
    "text": "frame = enter_call_expr(expr; enter_generated=false)\n\nBuild a Frame ready to execute the expression expr. Set enter_generated=true if you want to execute the generator of a @generated function, rather than the code that would be created by the generator.\n\nExample\n\njulia> mymethod(x) = x+1\nmymethod (generic function with 1 method)\n\njulia> JuliaInterpreter.enter_call_expr(:($mymethod(1)))\nFrame for mymethod(x) in Main at none:1\n  1* 1  1 ─ %1 = (+)(x, 1)\n  2  1  └──      return %1\nx = 1\n\njulia> mymethod(x::Vector{T}) where T = 1\nmymethod (generic function with 2 methods)\n\njulia> a = [1.0, 2.0]\n2-element Array{Float64,1}:\n 1.0\n 2.0\n\njulia> JuliaInterpreter.enter_call_expr(:($mymethod($a)))\nFrame for mymethod(x::Array{T,1}) where T in Main at none:1\n  1* 1  1 ─     return 1\nx = [1.0, 2.0]\nT = Float64\n\nSee enter_call for a similar approach not based on expressions.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.prepare_frame",
    "page": "Function reference",
    "title": "JuliaInterpreter.prepare_frame",
    "category": "function",
    "text": "frame = prepare_frame(framecode::FrameCode, frameargs, lenv)\n\nConstruct a new Frame for framecode, given lowered-code arguments frameargs and static parameters lenv. See JuliaInterpreter.prepare_call for information about how to prepare the inputs.\n\n\n\n\n\n"
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
    "text": "framecode, frameargs, lenv, argtypes = prepare_call(f, allargs; enter_generated=false)\n\nPrepare all the information needed to execute lowered code for f given arguments allargs. f and allargs are the outputs of prepare_args. For @generated methods, set enter_generated=true if you want to extract the lowered code of the generator itself.\n\nOn return framecode is the FrameCode of the method. frameargs contains the actual arguments needed for executing this frame (for generators, this will be the types of allargs); lenv is the \"environment\", i.e., the static parameters for f given allargs. argtypes is the Tuple-type for this specific call (equivalent to the signature of the MethodInstance).\n\nExample\n\njulia> mymethod(x::Vector{T}) where T = 1\nmymethod (generic function with 1 method)\n\njulia> framecode, frameargs, lenv, argtypes = JuliaInterpreter.prepare_call(mymethod, [mymethod, [1.0,2.0]]);\n\njulia> framecode\n  1  1  1 ─     return 1\n\njulia> frameargs\n2-element Array{Any,1}:\n mymethod\n [1.0, 2.0]\n\njulia> lenv\nsvec(Float64)\n\njulia> argtypes\nTuple{typeof(mymethod),Array{Float64,1}}\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.prepare_thunk",
    "page": "Function reference",
    "title": "JuliaInterpreter.prepare_thunk",
    "category": "function",
    "text": "frame = prepare_thunk(mod::Module, expr::Expr)\n\nPrepare expr for evaluation in mod. expr should be a \"straightforward\" expression, one that does not require special top-level handling (see JuliaInterpreter.split_expressions).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.split_expressions",
    "page": "Function reference",
    "title": "JuliaInterpreter.split_expressions",
    "category": "function",
    "text": "modexs, docexprs = split_expressions(mod::Module, expr::Expr; extract_docexprs=false)\n\nBreak expr into a list modexs of sequential blocks. This is often needed when expr needs to be evaluated at top level.\n\nmodexs[i] is a (mod::Module, ex::Expr) tuple, where ex is to be evaluated in mod.\n\nToplevel evaluation\n\nFor code that defines new structs, new methods, or new macros, it can be important to evaluate these expressions carefully:\n\nstack = Frame[]\nfor modex in modexs    # or use `for (mod, ex) in modexs` to split the tuple\n    frame = JuliaInterpreter.prepare_thunk(modex)\n    while true\n        JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break\n    end\nend\n\nThe while loop here deserves some explanation. Occasionally, a frame may define new methods (e.g., anonymous or local functions) and then call those methods. In such cases, running the entire frame as a single block (e.g., with JuliaInterpreter.finish_and_return! can trigger \"method is too new...\" errors. Instead, the approach above runs each frame, but returns to the caller after any new method is defined. When this loop is running at top level (e.g., in the REPL), this allows the world age to update and thus avoid \"method is too new...\" errors.\n\nPutting the above nested loop inside a function defeats its purpose, because inside a compiled function the world age will not update. If necessary, use the following strategy:\n\nCore.eval(somemodule, Expr(:toplevel, quote\n    body\n))\n\nwhere body contains the nested loop, plus any preparatory statements required to make the necessary variables available at top level in somemodule.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.get_call_framecode",
    "page": "Function reference",
    "title": "JuliaInterpreter.get_call_framecode",
    "category": "function",
    "text": "framecode, lenv = get_call_framecode(fargs, parentframe::FrameCode, idx::Int)\n\nReturn the framecode and environment for a call specified by fargs = [f, args...] (see prepare_args). parentframecode is the caller, and idx is the program-counter index. If possible, framecode will be looked up from the local method tables of parentframe.\n\n\n\n\n\n"
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
    "text": "JuliaInterpreter.enter_call\nJuliaInterpreter.enter_call_expr\nJuliaInterpreter.prepare_frame\nJuliaInterpreter.determine_method_for_expr\nJuliaInterpreter.prepare_args\nJuliaInterpreter.prepare_call\nJuliaInterpreter.prepare_thunk\nJuliaInterpreter.split_expressions\nJuliaInterpreter.get_call_framecode\nJuliaInterpreter.optimize!"
},

{
    "location": "dev_reference/#JuliaInterpreter.root",
    "page": "Function reference",
    "title": "JuliaInterpreter.root",
    "category": "function",
    "text": "rframe = root(frame)\n\nReturn the initial frame in the call stack.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.leaf",
    "page": "Function reference",
    "title": "JuliaInterpreter.leaf",
    "category": "function",
    "text": "lframe = leaf(frame)\n\nReturn the deepest callee in the call stack.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Frame-traversal-1",
    "page": "Function reference",
    "title": "Frame traversal",
    "category": "section",
    "text": "root\nleaf"
},

{
    "location": "dev_reference/#JuliaInterpreter.Compiled",
    "page": "Function reference",
    "title": "JuliaInterpreter.Compiled",
    "category": "type",
    "text": "Compiled is a trait indicating that any :call expressions should be evaluated using Julia\'s normal compiled-code evaluation. The alternative is to pass stack=Frame[], which will cause all calls to be evaluated via the interpreter.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.step_expr!",
    "page": "Function reference",
    "title": "JuliaInterpreter.step_expr!",
    "category": "function",
    "text": "pc = step_expr!(recurse, frame, istoplevel=false)\npc = step_expr!(frame, istoplevel=false)\n\nExecute the next statement in frame. pc is the new program counter, or nothing if execution terminates, or a BreakpointRef if execution hits a breakpoint.\n\nrecurse controls call evaluation; recurse = Compiled() evaluates :call expressions by normal dispatch. The default value recurse = finish_and_return! will use recursive interpretation.\n\nIf you are evaluating frame at module scope you should pass istoplevel=true.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.finish!",
    "page": "Function reference",
    "title": "JuliaInterpreter.finish!",
    "category": "function",
    "text": "pc = finish!(recurse, frame, istoplevel=false)\npc = finish!(frame, istoplevel=false)\n\nRun frame until execution terminates. pc is either nothing (if execution terminates when it hits a return statement) or a reference to a breakpoint. In the latter case, leaf(frame) returns the frame in which it hit the breakpoint.\n\nrecurse controls call evaluation; recurse = Compiled() evaluates :call expressions by normal dispatch, whereas the default recurse = finish_and_return! uses recursive interpretation.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.finish_and_return!",
    "page": "Function reference",
    "title": "JuliaInterpreter.finish_and_return!",
    "category": "function",
    "text": "ret = finish_and_return!(recurse, frame, istoplevel::Bool=false)\nret = finish_and_return!(frame, istoplevel::Bool=false)\n\nCall JuliaInterpreter.finish! and pass back the return value ret. If execution pauses at a breakpoint, ret is the reference to the breakpoint.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.finish_stack!",
    "page": "Function reference",
    "title": "JuliaInterpreter.finish_stack!",
    "category": "function",
    "text": "ret = finish_stack!(recurse, frame, rootistoplevel=false)\nret = finish_stack!(frame, rootistoplevel=false)\n\nUnwind the callees of frame, finishing each before returning to the caller. frame itself is also finished. rootistoplevel should be true if the root frame is top-level.\n\nret is typically the returned value. If execution hits a breakpoint, ret will be a reference to the breakpoint.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.get_return",
    "page": "Function reference",
    "title": "JuliaInterpreter.get_return",
    "category": "function",
    "text": "ret = get_return(frame)\n\nGet the return value of frame. Throws an error if frame.pc does not point to a return expression. frame must have already been executed so that the return value has been computed (see, e.g., JuliaInterpreter.finish!).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.next_until!",
    "page": "Function reference",
    "title": "JuliaInterpreter.next_until!",
    "category": "function",
    "text": "pc = next_until!(predicate, recurse, frame, istoplevel=false)\npc = next_until!(predicate, frame, istoplevel=false)\n\nExecute the current statement. Then step through statements of frame until the next statement satisfies predicate(frame). pc will be the index of the statement at which evaluation terminates, nothing (if the frame reached a return), or a BreakpointRef.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.maybe_next_until!",
    "page": "Function reference",
    "title": "JuliaInterpreter.maybe_next_until!",
    "category": "function",
    "text": "pc = maybe_next_until!(predicate, recurse, frame, istoplevel=false)\npc = maybe_next_until!(predicate, frame, istoplevel=false)\n\nLike next_until! except checks predicate before executing the current statment.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.through_methoddef_or_done!",
    "page": "Function reference",
    "title": "JuliaInterpreter.through_methoddef_or_done!",
    "category": "function",
    "text": "pc = through_methoddef_or_done!(recurse, frame)\npc = through_methoddef_or_done!(frame)\n\nRuns frame at top level until it either finishes (e.g., hits a return statement) or defines a new method.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.evaluate_call!",
    "page": "Function reference",
    "title": "JuliaInterpreter.evaluate_call!",
    "category": "function",
    "text": "ret = evaluate_call!(Compiled(), frame::Frame, call_expr)\nret = evaluate_call!(recurse,    frame::Frame, call_expr)\n\nEvaluate a :call expression call_expr in the context of frame. The first causes it to be executed using Julia\'s normal dispatch (compiled code), whereas the second recurses in via the interpreter. recurse has a default value of JuliaInterpreter.finish_and_return!.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.evaluate_foreigncall",
    "page": "Function reference",
    "title": "JuliaInterpreter.evaluate_foreigncall",
    "category": "function",
    "text": "ret = evaluate_foreigncall(frame::Frame, call_expr)\n\nEvaluate a :foreigncall (from a ccall) statement callexpr in the context of frame.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.maybe_evaluate_builtin",
    "page": "Function reference",
    "title": "JuliaInterpreter.maybe_evaluate_builtin",
    "category": "function",
    "text": "ret = maybe_evaluate_builtin(frame, call_expr, expand::Bool)\n\nIf call_expr is to a builtin function, evaluate it, returning the result inside a Some wrapper. Otherwise, return call_expr.\n\nIf expand is true, Core._apply calls will be resolved as a call to the applied function.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.next_call!",
    "page": "Function reference",
    "title": "JuliaInterpreter.next_call!",
    "category": "function",
    "text": "pc = next_call!(recurse, frame, istoplevel=false)\npc = next_call!(frame, istoplevel=false)\n\nExecute the current statement. Continue stepping through frame until the next :return or :call expression.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.maybe_next_call!",
    "page": "Function reference",
    "title": "JuliaInterpreter.maybe_next_call!",
    "category": "function",
    "text": "pc = maybe_next_call!(recurse, frame, istoplevel=false)\npc = maybe_next_call!(frame, istoplevel=false)\n\nReturn the current program counter of frame if it is a :return or :call expression. Otherwise, step through the statements of frame until the next :return or :call expression.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.next_line!",
    "page": "Function reference",
    "title": "JuliaInterpreter.next_line!",
    "category": "function",
    "text": "pc = next_line!(recurse, frame, istoplevel=false)\npc = next_line!(frame, istoplevel=false)\n\nExecute until reaching the first call of the next line of the source code. Upon return, pc is either the new program counter, nothing if a return is reached, or a BreakpointRef if it encountered a wrapper call. In the latter case, call leaf(frame) to obtain the new execution frame.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.until_line!",
    "page": "Function reference",
    "title": "JuliaInterpreter.until_line!",
    "category": "function",
    "text": "pc = until_line!(recurse, frame, line=nothing istoplevel=false)\npc = until_line!(frame, line=nothing, istoplevel=false)\n\nExecute until the current frame reaches a line greater than line. If line == nothing execute until the current frame reaches any line greater than the current line.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.maybe_reset_frame!",
    "page": "Function reference",
    "title": "JuliaInterpreter.maybe_reset_frame!",
    "category": "function",
    "text": "ret = maybe_reset_frame!(recurse, frame, pc, rootistoplevel)\n\nPerform a return to the caller, or descend to the level of a breakpoint. pc is the return state from the previous command (e.g., next_call! or similar). rootistoplevel should be true if the root frame is top-level.\n\nret will be nothing if we have just completed a top-level frame. Otherwise,\n\ncframe, cpc = ret\n\nwhere cframe is the frame from which execution should continue and cpc is the state of cframe (the program counter, a BreakpointRef, or nothing).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.maybe_step_through_wrapper!",
    "page": "Function reference",
    "title": "JuliaInterpreter.maybe_step_through_wrapper!",
    "category": "function",
    "text": "cframe = maybe_step_through_wrapper!(recurse, frame)\ncframe = maybe_step_through_wrapper!(frame)\n\nReturn the new frame of execution, potentially stepping through \"wrapper\" methods like those that supply default positional arguments or handle keywords. cframe is the leaf frame from which execution should start.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.maybe_step_through_kwprep!",
    "page": "Function reference",
    "title": "JuliaInterpreter.maybe_step_through_kwprep!",
    "category": "function",
    "text": "frame = maybe_step_through_kwprep!(recurse, frame)\nframe = maybe_step_through_kwprep!(frame)\n\nIf frame.pc points to the beginning of preparatory work for calling a keyword-argument function, advance forward until the actual call.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.handle_err",
    "page": "Function reference",
    "title": "JuliaInterpreter.handle_err",
    "category": "function",
    "text": "loc = handle_err(recurse, frame, err)\n\nDeal with an error err that arose while evaluating frame. There are one of three behaviors:\n\nif frame catches the error, loc is the program counter at which to resume evaluation of frame;\nif frame doesn\'t catch the error, but break_on_error[] is true, loc is a BreakpointRef;\notherwise, err gets rethrown.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.debug_command",
    "page": "Function reference",
    "title": "JuliaInterpreter.debug_command",
    "category": "function",
    "text": "ret = debug_command(recurse, frame, cmd, rootistoplevel=false; line=nothing)\nret = debug_command(frame, cmd, rootistoplevel=false; line=nothing)\n\nPerform one \"debugger\" command. The keyword arguments are not used for all debug commands. cmd should be one of:\n\n:n: advance to the next line\n:s: step into the next call\n:until: advance the frame to line line if given, otherwise advance to the line after the current line\n:c: continue execution until termination or reaching a breakpoint\n:finish: finish the current frame and return to the parent\n\nor one of the \'advanced\' commands\n\n:nc: step forward to the next call\n:se: execute a single statement\n:si: execute a single statement, stepping in if it\'s a call\n:sg: step into the generator of a generated function\n\nrootistoplevel and ret are as described for JuliaInterpreter.maybe_reset_frame!.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Frame-execution-1",
    "page": "Function reference",
    "title": "Frame execution",
    "category": "section",
    "text": "JuliaInterpreter.Compiled\nJuliaInterpreter.step_expr!\nJuliaInterpreter.finish!\nJuliaInterpreter.finish_and_return!\nJuliaInterpreter.finish_stack!\nJuliaInterpreter.get_return\nJuliaInterpreter.next_until!\nJuliaInterpreter.maybe_next_until!\nJuliaInterpreter.through_methoddef_or_done!\nJuliaInterpreter.evaluate_call!\nJuliaInterpreter.evaluate_foreigncall\nJuliaInterpreter.maybe_evaluate_builtin\nJuliaInterpreter.next_call!\nJuliaInterpreter.maybe_next_call!\nJuliaInterpreter.next_line!\nJuliaInterpreter.until_line!\nJuliaInterpreter.maybe_reset_frame!\nJuliaInterpreter.maybe_step_through_wrapper!\nJuliaInterpreter.maybe_step_through_kwprep!\nJuliaInterpreter.handle_err\nJuliaInterpreter.debug_command"
},

{
    "location": "dev_reference/#JuliaInterpreter.@breakpoint",
    "page": "Function reference",
    "title": "JuliaInterpreter.@breakpoint",
    "category": "macro",
    "text": "@breakpoint f(args...) condition=nothing\n@breakpoint f(args...) line condition=nothing\n\nBreak upon entry, or at the specified line number, in the method called by f(args...). Optionally supply a condition expressed in terms of the arguments and internal variables of the method. If line is supplied, it must be a literal integer.\n\nExample\n\nSuppose a method mysum is defined as follows, where the numbers to the left are the line number in the file:\n\n12 function mysum(A)\n13     s = zero(eltype(A))\n14     for a in A\n15         s += a\n16     end\n17     return s\n18 end\n\nThen\n\n@breakpoint mysum(A) 15 s>10\n\nwould cause execution of the loop to break whenever s>10.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.@bp",
    "page": "Function reference",
    "title": "JuliaInterpreter.@bp",
    "category": "macro",
    "text": "@bp\n\nInsert a breakpoint at a location in the source code.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.breakpoint",
    "page": "Function reference",
    "title": "JuliaInterpreter.breakpoint",
    "category": "function",
    "text": "breakpoint(f, [sig], [line], [condition])\n\nAdd a breakpoint to f with the specified argument types sig.¨ If sig is not given, the breakpoint will apply to all methods of f. If f is a method, the breakpoint will only apply to that method. Optionally specify an absolute line number line in the source file; the default is to break upon entry at the first line of the body. Without condition, the breakpoint will be triggered every time it is encountered; the second only if condition evaluates to true. condition should be written in terms of the arguments and local variables of f.\n\nExample\n\nfunction radius2(x, y)\n    return x^2 + y^2\nend\n\nbreakpoint(radius2, Tuple{Int,Int}, :(y > x))\n\n\n\n\n\nbreakpoint(file, line, [condition])\n\nSet a breakpoint in file at line. The argument file can be a filename, a partial path or absolute path. For example, file = foo.jl will match against all files with the name foo.jl, file = src/foo.jl will match against all paths containing src/foo.jl, e.g. both Foo/src/foo.jl and Bar/src/foo.jl. Absolute paths only matches against the file with that exact absolute path.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.enable",
    "page": "Function reference",
    "title": "JuliaInterpreter.enable",
    "category": "function",
    "text": "enable(bp::AbstractBreakpoint)\n\nEnable breakpoint bp.\n\n\n\n\n\nenable()\n\nEnable all breakpoints.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.disable",
    "page": "Function reference",
    "title": "JuliaInterpreter.disable",
    "category": "function",
    "text": "disable(bp::AbstractBreakpoint)\n\nDisable breakpoint bp. Disabled breakpoints can be re-enabled with enable.\n\n\n\n\n\ndisable()\n\nDisable all breakpoints.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.remove",
    "page": "Function reference",
    "title": "JuliaInterpreter.remove",
    "category": "function",
    "text": "remove(bp::AbstractBreakpoint)\n\nRemove (delete) breakpoint bp. Removed breakpoints cannot be re-enabled.\n\n\n\n\n\nremove()\n\nRemove all breakpoints.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.toggle",
    "page": "Function reference",
    "title": "JuliaInterpreter.toggle",
    "category": "function",
    "text": "toggle(bp::AbstractBreakpoint)\n\nToggle breakpoint bp.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.break_on",
    "page": "Function reference",
    "title": "JuliaInterpreter.break_on",
    "category": "function",
    "text": "break_on(states...)\n\nTurn on automatic breakpoints when any of the conditions described in states occurs. The supported states are:\n\n:error: trigger a breakpoint any time an uncaught exception is thrown\n:throw : trigger a breakpoint any time a throw is executed (even if it will eventually be caught)\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.break_off",
    "page": "Function reference",
    "title": "JuliaInterpreter.break_off",
    "category": "function",
    "text": "break_off(states...)\n\nTurn off automatic breakpoints when any of the conditions described in states occurs. See break_on for a description of valid states.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.breakpoints",
    "page": "Function reference",
    "title": "JuliaInterpreter.breakpoints",
    "category": "function",
    "text": "breakpoints()::Vector{AbstractBreakpoint}\n\nReturn an array with all breakpoints.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.dummy_breakpoint",
    "page": "Function reference",
    "title": "JuliaInterpreter.dummy_breakpoint",
    "category": "function",
    "text": "bpref = dummy_breakpoint(recurse, frame::Frame, istoplevel)\n\nReturn a fake breakpoint. dummy_breakpoint can be useful as the recurse argument to evaluate_call! (or any of the higher-order commands) to ensure that you return immediately after stepping into a call.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Breakpoints-1",
    "page": "Function reference",
    "title": "Breakpoints",
    "category": "section",
    "text": "@breakpoint\n@bp\nbreakpoint\nenable\ndisable\nremove\ntoggle\nbreak_on\nbreak_off\nbreakpoints\nJuliaInterpreter.dummy_breakpoint"
},

{
    "location": "dev_reference/#JuliaInterpreter.Frame",
    "page": "Function reference",
    "title": "JuliaInterpreter.Frame",
    "category": "type",
    "text": "Frame represents the current execution state in a particular call frame. Fields:\n\nframecode: the [FrameCode] for this frame.\nframedata: the [FrameData] for this frame.\npc: the program counter (integer index of the next statment to be evaluated) for this frame.\ncaller: the parent caller of this frame, or nothing.\ncallee: the frame called by this one, or nothing.\n\nThe Base functions show_backtrace and display_error are overloaded such that show_backtrace(io::IO, frame::Frame) and display_error(io::IO, er, frame::Frame) shows a backtrace or error, respectively, in a similar way as to how Base shows them.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.FrameCode",
    "page": "Function reference",
    "title": "JuliaInterpreter.FrameCode",
    "category": "type",
    "text": "FrameCode holds static information about a method or toplevel code. One FrameCode can be shared by many calling Frames.\n\nImportant fields:\n\nscope: the Method or Module in which this frame is to be evaluated.\nsrc: the CodeInfo object storing (optimized) lowered source code.\nmethodtables: a vector, each entry potentially stores a \"local method table\" for the corresponding :call expression in src (undefined entries correspond to statements that do not contain :call expressions).\nused: a BitSet storing the list of SSAValues that get referenced by later statements.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.FrameData",
    "page": "Function reference",
    "title": "JuliaInterpreter.FrameData",
    "category": "type",
    "text": "FrameData holds the arguments, local variables, and intermediate execution state in a particular call frame.\n\nImportant fields:\n\nlocals: a vector containing the input arguments and named local variables for this frame. The indexing corresponds to the names in the slotnames of the src. Use locals to extract the current value of local variables.\nssavalues: a vector containing the Static Single Assignment values produced at the current state of execution.\nsparams: the static type parameters, e.g., for f(x::Vector{T}) where T this would store the value of T given the particular input x.\nexception_frames: a list of indexes to catch blocks for handling exceptions within the current frame. The active handler is the last one on the list.\nlast_exception: the exception thrown by this frame or one of its callees.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.FrameInstance",
    "page": "Function reference",
    "title": "JuliaInterpreter.FrameInstance",
    "category": "type",
    "text": "FrameInstance represents a method specialized for particular argument types.\n\nFields:\n\nframecode: the FrameCode for the method.\nsparam_vals: the static parameter values for the method.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.BreakpointState",
    "page": "Function reference",
    "title": "JuliaInterpreter.BreakpointState",
    "category": "type",
    "text": "BreakpointState(isactive=true, condition=JuliaInterpreter.truecondition)\n\nBreakpointState represents a breakpoint at a particular statement in a FrameCode. isactive indicates whether the breakpoint is currently enabled or disabled. condition is a function that accepts a single Frame, and condition(frame) must return either true or false. Execution will stop at a breakpoint only if isactive and condition(frame) both evaluate as true. The default condition always returns true.\n\nTo create these objects, see breakpoint.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.BreakpointRef",
    "page": "Function reference",
    "title": "JuliaInterpreter.BreakpointRef",
    "category": "type",
    "text": "BreakpointRef(framecode, stmtidx)\nBreakpointRef(framecode, stmtidx, err)\n\nA reference to a breakpoint at a particular statement index stmtidx in framecode. If the break was due to an error, supply that as well.\n\nCommands that execute complex control-flow (e.g., next_line!) may also return a BreakpointRef to indicate that the execution stack switched frames, even when no breakpoint has been set at the corresponding statement.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.AbstractBreakpoint",
    "page": "Function reference",
    "title": "JuliaInterpreter.AbstractBreakpoint",
    "category": "type",
    "text": "AbstractBreakpoint is the abstract type that is the supertype for breakpoints. Currently, the concrete breakpoint types BreakpointSignature and BreakpointFileLocation exist.\n\nCommon fields shared by the concrete breakpoints:\n\ncondition::Union{Nothing,Expr,Tuple{Module,Expr}}: the condition when the breakpoint applies . nothing means unconditionally, otherwise when the Expr (optionally in Module).\nenabled::Ref{Bool}: If the breakpoint is enabled (should not be directly modified, use enable() or disable()).\ninstances::Vector{BreakpointRef}: All the BreakpointRef that the breakpoint has applied to.\nline::Int The line of the breakpoint (equal to 0 if unset).\n\nSee BreakpointSignature and BreakpointFileLocation for additional fields in the concrete types.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.BreakpointSignature",
    "page": "Function reference",
    "title": "JuliaInterpreter.BreakpointSignature",
    "category": "type",
    "text": "A BreakpointSignature is a breakpoint that is set on methods or functions.\n\nFields:\n\nf::Union{Method, Function}: A method or function that the breakpoint should apply to.\nsig::Union{Nothing, Type}: if f is a Method, always equal to nothing. Otherwise, contains the method signature  as a tuple type for what methods the breakpoint should apply to.\n\nFor common fields shared by all breakpoints, see AbstractBreakpoint.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.BreakpointFileLocation",
    "page": "Function reference",
    "title": "JuliaInterpreter.BreakpointFileLocation",
    "category": "type",
    "text": "A BreakpointFileLocation is a breakpoint that is set on a line in a file.\n\nFields:\n\npath::String: The literal string that was used to create the breakpoint, e.g. \"path/file.jl\".\nabspath::String: The absolute path to the file when the breakpoint was created, e.g. \"/Users/Someone/path/file.jl\".\n\nFor common fields shared by all breakpoints, see AbstractBreakpoint.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Types-1",
    "page": "Function reference",
    "title": "Types",
    "category": "section",
    "text": "JuliaInterpreter.Frame\nJuliaInterpreter.FrameCode\nJuliaInterpreter.FrameData\nJuliaInterpreter.FrameInstance\nJuliaInterpreter.BreakpointState\nJuliaInterpreter.BreakpointRef\nJuliaInterpreter.AbstractBreakpoint\nJuliaInterpreter.BreakpointSignature\nJuliaInterpreter.BreakpointFileLocation"
},

{
    "location": "dev_reference/#JuliaInterpreter.framedict",
    "page": "Function reference",
    "title": "JuliaInterpreter.framedict",
    "category": "constant",
    "text": "framedict[method] returns the FrameCode for method. For @generated methods, see genframedict.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.genframedict",
    "page": "Function reference",
    "title": "JuliaInterpreter.genframedict",
    "category": "constant",
    "text": "genframedict[(method,argtypes)] returns the FrameCode for a @generated method method, for the particular argument types argtypes.\n\nThe framecodes stored in genframedict are for the code returned by the generator (i.e, what will run when you call the method on particular argument types); for the generator itself, its framecode would be stored in framedict.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.compiled_methods",
    "page": "Function reference",
    "title": "JuliaInterpreter.compiled_methods",
    "category": "constant",
    "text": "meth ∈ compiled_methods indicates that meth should be run using Compiled rather than recursed into via the interpreter.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.compiled_modules",
    "page": "Function reference",
    "title": "JuliaInterpreter.compiled_modules",
    "category": "constant",
    "text": "mod ∈ compiled_modules indicates that any method in mod should be run using Compiled rather than recursed into via the interpreter.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Internal-storage-1",
    "page": "Function reference",
    "title": "Internal storage",
    "category": "section",
    "text": "JuliaInterpreter.framedict\nJuliaInterpreter.genframedict\nJuliaInterpreter.compiled_methods\nJuliaInterpreter.compiled_modules"
},

{
    "location": "dev_reference/#JuliaInterpreter.@lookup",
    "page": "Function reference",
    "title": "JuliaInterpreter.@lookup",
    "category": "macro",
    "text": "rhs = @lookup(frame, node)\nrhs = @lookup(mod, frame, node)\n\nThis macro looks up previously-computed values referenced as SSAValues, SlotNumbers, GlobalRefs, QuoteNode, sparam or exception reference expression. It will also lookup symbols in moduleof(frame); this can be supplied ahead-of-time via the 3-argument version. If none of the above apply, the value of node will be returned.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.is_wrapper_call",
    "page": "Function reference",
    "title": "JuliaInterpreter.is_wrapper_call",
    "category": "function",
    "text": "Determine whether we are calling a function for which the current function is a wrapper (either because of optional arguments or because of keyword arguments).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.is_doc_expr",
    "page": "Function reference",
    "title": "JuliaInterpreter.is_doc_expr",
    "category": "function",
    "text": "is_doc_expr(ex)\n\nTest whether expression ex is a @doc expression.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.is_global_ref",
    "page": "Function reference",
    "title": "JuliaInterpreter.is_global_ref",
    "category": "function",
    "text": "is_global_ref(g, mod, name)\n\nTests whether g is equal to GlobalRef(mod, name).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#CodeTracking.whereis",
    "page": "Function reference",
    "title": "CodeTracking.whereis",
    "category": "function",
    "text": "loc = whereis(frame, pc=frame.pc)\n\nReturn the file and line number for frame at pc.  If this cannot be determined, loc == nothing. Otherwise loc == (filepath, line).\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.linenumber",
    "page": "Function reference",
    "title": "JuliaInterpreter.linenumber",
    "category": "function",
    "text": "line = linenumber(framecode, pc)\n\nReturn the \"static\" line number at statement index pc. The static line number is the location at the time the method was most recently defined. See CodeTracking.whereis for dynamic line information.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.statementnumber",
    "page": "Function reference",
    "title": "JuliaInterpreter.statementnumber",
    "category": "function",
    "text": "stmtidx = statementnumber(frame, line)\n\nReturn the index of the first statement in frame\'s CodeInfo that corresponds to static line number line.\n\n\n\n\n\nframecode, stmtidx = statementnumber(method, line)\n\nReturn the index of the first statement in framecode that corresponds to the given static line number line in method.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.Variable",
    "page": "Function reference",
    "title": "JuliaInterpreter.Variable",
    "category": "type",
    "text": "Variable is a struct representing a variable with an asigned value. By calling the function locals[@ref] on a Frame[@ref] a Vector of Variable\'s is returned.\n\nImportant fields:\n\nvalue::Any: the value of the local variable.\nname::Symbol: the name of the variable as given in the source code.\nisparam::Bool: if the variable is a type parameter, for example T in f(x::T) where {T} = x.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.locals",
    "page": "Function reference",
    "title": "JuliaInterpreter.locals",
    "category": "function",
    "text": "local_variables = locals(frame::Frame)::Vector{Variable}\n\nReturn the local variables as a vector of Variable[@ref].\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#JuliaInterpreter.whichtt",
    "page": "Function reference",
    "title": "JuliaInterpreter.whichtt",
    "category": "function",
    "text": "method = whichtt(tt)\n\nLike which except it operates on the complete tuple-type tt.\n\n\n\n\n\n"
},

{
    "location": "dev_reference/#Utilities-1",
    "page": "Function reference",
    "title": "Utilities",
    "category": "section",
    "text": "JuliaInterpreter.@lookup\nJuliaInterpreter.is_wrapper_call\nJuliaInterpreter.is_doc_expr\nJuliaInterpreter.is_global_ref\nCodeTracking.whereis\nJuliaInterpreter.linenumber\nJuliaInterpreter.statementnumber\nJuliaInterpreter.Variable\nJuliaInterpreter.locals\nJuliaInterpreter.whichtt"
},

]}
