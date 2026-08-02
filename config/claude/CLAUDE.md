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

Nushell renders lists and tables as bordered "fancy" tables by default. `head` and `tail` aren't Nushell builtins — they're external coreutils — so piping structured data to them makes Nushell render the value to that table text first; the tools then slice the box-drawing display (top border, header, separator…), not the underlying rows. Use `first`/`last` for the first or last *n* rows, and `get <i>` for the *n*th.

When unsure a construct works in nushell, prefer the explicit nushell form over guessing.

# Model Usage & Delegation

My session model is always an expensive tier, and I want those tokens spent on reasoning, not plumbing. Act as the orchestrator: keep decomposition, design decisions, hard debugging, integration, and final synthesis in the main loop; push execution-heavy, reasoning-light work to subagents.

**Never spawn a subagent without an explicit tier.** An Agent call with no `model` inherits the session model, so that delegation saves nothing. Prefer a named agent over `general-purpose` plus a `model:` param — each one pins its own tier and its description says when it applies:

| agent                 | tier   | for                                                                                    |
|-----------------------|--------|----------------------------------------------------------------------------------------|
| `scout`               | haiku  | repo-local lookups: where something is defined, call sites, does this pattern exist    |
| `host-inspector`      | sonnet | read-only SSH triage on a remote NixOS host: journals, units, podman, ZFS, DNS         |
| `nix-archaeologist`   | sonnet | evaluating the Nix graph: option provenance, what an expression evaluates to, closures |
| `implementer`         | sonnet | scoped implementation where the design is settled                                      |
| `upstream-researcher` | sonnet | ground truth about a third-party project from its own source                           |
| `verifier`            | opus   | adversarial verification of one correctness-critical claim                             |

For ad-hoc calls that don't fit one of those, still set `model` deliberately: `haiku` mechanical and fully specified, `sonnet` scoped work needing some judgment, `opus` hard reasoning that stands alone from the main thread. Omit it only when the subtask needs top-tier reasoning *and* is inseparable from the session's accumulated context. Workflow scripts set `model`/`effort` per stage.

**The test:** does this need my reasoning, or just hands and context space? Work that would pull large file dumps or logs into the main context but only needs a conclusion is a delegation candidate even when it's easy.

**Don't delegate** when writing the handoff prompt costs more than the task itself, when the task depends on accumulated conversation context that's expensive to restate, or when it's correctness-critical and hard to verify.

**You own delegated results.** Spot-check subagent output against the actual code before building on it or relaying it to me. If a cheaper model comes back wrong or useless once, redo that subtask one tier up instead of retrying the same tier.

# Code Comments

Write comments for the next person who reads this code in its committed, finished state — not as a log of how you arrived at it.

- Comment the *why*, not the *what*. Explain intent, constraints, and non-obvious reasoning. Don't narrate what the code plainly says.
- No process or conversational comments. Never leave notes about the change you're making, alternatives you considered, what the code does "now" versus before, instructions you were given, or who/what made the decision. If a comment only makes sense in the context of our conversation, it doesn't belong in the file.
- Reserve comments for things that genuinely help a future reader: an unusual or surprising decision, a workaround for an external bug or constraint, a subtle invariant, or a warning about a non-obvious consequence.
- Prefer fewer, durable comments over many situational ones. When in doubt, leave it out — clear naming and structure beat a comment.
- Match the surrounding code's existing comment style and density.
