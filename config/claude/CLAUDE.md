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

My default shell on every machine is **Nushell** (`nu`). Any shell command you hand me to run myself — anything in a code block I'm meant to copy/paste or read as "run this" — must be nushell syntax.

**Self-check before writing any command block:** does it use a variable, a pipe, an env var, redirection, globbing, quoting, or control flow? If yes → nushell, no exceptions. Those are exactly the spots where bash habits leak in.

- Variables: `let x = ...` / `$x`, not `x=...`.
- Pipelines: nushell ops (`| where`, `| get`, `| select`, `| from json`), not `grep`/`awk`/`sed`/`jq` chains on structured data.
- External command behind a builtin name: `^cmd`.
- Interpolation `$"..."`; subshell `(...)`, not `$(...)`.
- Env: `$env.FOO = "bar"` or `with-env { FOO: bar } { ... }`, not `export FOO=bar`.
- Conditionals `if ... { ... } else { ... }`; no `[[ ... ]]`, no `&&`/`||` chaining (use `if` or pipeline error handling).

This rule governs commands handed to **me**. It is NOT about the Bash *tool* you use for your own work — there a single external invocation (`git status`, `nix build .#foo`) is identical in both shells and bash is fine. The leak happens when you carry that bash-tool default into a command you write for me. Bash in a handoff command is only OK when I explicitly ask for it.

**Every machine I own runs nushell, including remote hosts.** When you run a command over SSH from the Bash tool, the `ssh` call itself is a local external invocation, but the command *string* you send executes in the remote login shell — which is nushell. So `ssh host '<cmd>'` requires `<cmd>` to be nushell syntax, even though the `ssh` wrapper is bash. The same applies to anything that ships a command to a remote machine for execution (`deploy-rs` run hooks, remote `nix` activation scripts, etc.). If you need a POSIX shell on the far end, invoke it explicitly (`ssh host 'bash -c "..."'`).

When unsure a construct works in nushell, prefer the explicit nushell form over guessing.

# Code Comments

Write comments for the next person who reads this code in its committed, finished state — not as a log of how you arrived at it.

- Comment the *why*, not the *what*. Explain intent, constraints, and non-obvious reasoning. Don't narrate what the code plainly says.
- No process or conversational comments. Never leave notes about the change you're making, alternatives you considered, what the code does "now" versus before, instructions you were given, or who/what made the decision. If a comment only makes sense in the context of our conversation, it doesn't belong in the file.
- Reserve comments for things that genuinely help a future reader: an unusual or surprising decision, a workaround for an external bug or constraint, a subtle invariant, or a warning about a non-obvious consequence.
- Prefer fewer, durable comments over many situational ones. When in doubt, leave it out — clear naming and structure beat a comment.
- Match the surrounding code's existing comment style and density.
