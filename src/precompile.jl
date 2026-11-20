module var"#Internal"
public_fn(x::String) = false
end

PrecompileTools.@compile_workload let
    @interpret sum(rand(10))
    expr = quote
        public_fn(x::Integer) = true
        module Private
            private(y::String) = false
        end
        const threshold = 0.1
    end
    frame = Frame(var"#Internal", Expr(:toplevel, expr.args...))
    debug_command(frame, :c, true)
end
