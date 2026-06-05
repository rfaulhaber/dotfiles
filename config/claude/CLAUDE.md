# System Info

All my machines are Nix-managed — NixOS on Linux hosts and nix-darwin on macOS. Tools and commands may not be globally installed, and the available commands differ between hosts depending on which modules are enabled.

When a command is not found, do NOT attempt to install it with apt, brew, npm -g, or similar.
Instead, use `nix run nixpkgs#<package> -- <args>` to run it ephemerally, or `nix shell nixpkgs#<package>` if you need it available for multiple commands.

Common mappings:
- `jq` → `nix run nixpkgs#jq -- <args>`
- `ripgrep` → `nix run nixpkgs#ripgrep -- <args>` (provides `rg`)
- `fd` → `nix run nixpkgs#fd -- <args>`
- `tree` → `nix run nixpkgs#tree -- <args>`

If you encounter "command not found", retry with `nix run nixpkgs#<package> -- <args>`.
Do not attempt to modify the system configuration or use `nix-env`.

# Shell

My default interactive and scripting shell on every machine is **Nushell** (`nu`), not bash or zsh.

When you give me shell code to run — one-liners, pipelines, or scripts — write it in **nushell-compatible syntax**, not POSIX/bash. This includes:

- Use `let x = ...` for variables, not `x=...`; reference them as `$x`.
- Use nushell's structured pipelines (`| where`, `| get`, `| select`, `| from json`, etc.) instead of `grep`/`awk`/`sed`/`jq` chains when the data is structured.
- Use `^cmd` to invoke an external command when its name collides with a builtin, or when you need bypass nushell's parser.
- String interpolation uses `$"..."`; subshells use `(...)`, not `$(...)`.
- Environment variables are set with `$env.FOO = "bar"` (or `with-env { FOO: bar } { ... }` for scoped), not `export FOO=bar`.
- Conditionals use `if ... { ... } else { ... }`; there is no `[[ ... ]]` or `&&`/`||` between statements (use `if` or pipeline error handling).

Bash is fine **only** when:
- I explicitly ask for bash, or
- The command is a single external invocation with no shell control flow (e.g., `git status`, `nix build .#foo`) — these are identical in both shells.

If you're unsure whether a construct works in nushell, prefer the explicit nushell form over guessing.

# Code Comments

Write comments for the next person who reads this code in its committed, finished state — not as a log of how you arrived at it.

- Comment the *why*, not the *what*. Explain intent, constraints, and non-obvious reasoning. Don't narrate what the code plainly says.
- No process or conversational comments. Never leave notes about the change you're making, alternatives you considered, what the code does "now" versus before, instructions you were given, or who/what made the decision. If a comment only makes sense in the context of our conversation, it doesn't belong in the file.
- Reserve comments for things that genuinely help a future reader: an unusual or surprising decision, a workaround for an external bug or constraint, a subtle invariant, or a warning about a non-obvious consequence.
- Prefer fewer, durable comments over many situational ones. When in doubt, leave it out — clear naming and structure beat a comment.
- Match the surrounding code's existing comment style and density.
