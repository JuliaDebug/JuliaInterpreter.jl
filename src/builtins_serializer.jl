# Builtins and intrinsics could be indexed by something smaller than Int32,
# but since the system uses Int32 let's just use FIndexT for indexing everything.

@enum BuiltinToken::FIndexT begin
    tgetfield=FIndexT(1)
    tegal
end

builtin_tokens = Dict(Core.getfield => tgetfield,
                      Core.:(===) => tegal)
const builtin = Any[]

intrinsic_token(f) = FIndexT(Core.bitcast(Int32, f))
const intrinsic = Any[]

function fill_ftables()
    resize!(builtin, length(instances(BuiltinToken)))
    for (f, tok) in builtin_tokens
        builtin[Int(tok)] = f
    end
    for fname in names(Core.Intrinsics)
        f = getfield(Core.Intrinsics, fname)
        if f isa Core.IntrinsicFunction
            tok = intrinsic_token(f)+1
            if tok > length(intrinsic)
                resize!(intrinsic, tok)
            end
            intrinsic[tok] = f
        end
    end
    nothing
end
