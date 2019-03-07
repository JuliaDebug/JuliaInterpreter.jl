Base.show(io::IO, pc::JuliaProgramCounter) = print(io, "JuliaProgramCounter(", pc.next_stmt, ')')

function Base.show(io::IO, var::Variable)
    print(io, var.name, " = ")
    show(io, var.value)
end

function framecode_lines(src::CodeInfo)
    buf = IOBuffer()
    show(buf, src)
    code = filter!(split(String(take!(buf)), '\n')) do line
        !(line == "CodeInfo(" || line == ")" || isempty(line))
    end
    code .= replace.(code, Ref(r"\$\(QuoteNode\((.+?)\)\)" => s"\1"))
    return code
end

function show_stackloc(io::IO, stack, frame, pc=frame.pc[])
    indent = ""
    for f in stack
        println(io, indent, f.code.scope)
        indent *= "  "
    end
    println(io, indent, frame.code.scope, ", pc = ", convert(Int, pc))
end
function show_stackloc(io::IO, ::Compiled, frame, pc)
    println(io, "No stack, ::Compiled")
    println(io, frame.code.scope, ", pc = ", convert(Int, pc))
end
show_stackloc(stack, frame, pc=frame.pc[]) = show_stackloc(stderr, stack, frame, pc)

function Base.show(io::IO, bp::BreakpointRef)
    if checkbounds(Bool, bp.framecode.breakpoints, bp.stmtidx)
        lineno = linenumber(bp.framecode, bp.stmtidx)
        print(io, "breakpoint(", bp.framecode.scope, ", ", lineno)
    else
        print(io, "breakpoint(", bp.framecode.scope, ", %", bp.stmtidx)
    end
    if bp.err !== nothing
        print(io, ", ", bp.err)
    end
    print(io, ')')
end
