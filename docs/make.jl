using Documenter, ASTInterpreter2

makedocs(
    modules = [ASTInterpreter2],
    clean = false,
    format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
    sitename = "ASTInterpreter2.jl",
    authors = "Keno Fischer, Tim Holy, and others",
    linkcheck = !("skiplinks" in ARGS),
    pages = [
        "Home" => "index.md",
        "ast.md",
        "internals.md",
        "dev_reference.md",
    ],
)

deploydocs(
    repo = "github.com/JuliaDebug/ASTInterpreter2.jl.git",
)
