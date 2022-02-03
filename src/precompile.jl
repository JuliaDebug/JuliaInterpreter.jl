module __JIInternal__
public(x::String) = false
end

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    @interpret sum(rand(10))
    expr = quote
        public(x::Integer) = true
        module Private
            private(y::String) = false
        end
        const threshold = 0.1
    end
    for (mod, ex) in ExprSplitter(__JIInternal__, expr)
        frame = Frame(mod, ex)
        debug_command(frame, :c, true)
    end
end
