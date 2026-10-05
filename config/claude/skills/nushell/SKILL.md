---
name: nushell
description: Use when writing any shell command for the user to run (their shell is Nushell on every machine), writing or editing a .nu script (bin/*.nu, .github/scripts, writeNushellScriptBin), sending a command to a remote host over ssh (its login shell is Nushell), or querying structured data (JSON, YAML, TOML, CSV) where you would otherwise reach for jq or a Python one-off.
---

# Nushell

## Where Nushell is required

**Commands handed to the user.** Anything the user is meant to copy and run is Nushell, never
bash. These are the translations that leak:

| bash | Nushell |
|---|---|
| `x=1; echo $x` | `let x = 1; print $x` |
| `export FOO=bar` | `$env.FOO = "bar"` |
| `"$HOME/x"` | `$"($env.HOME)/x"` |
| `$(cmd)` | `(cmd)` |
| `cmd1 && cmd2` | `cmd1; cmd2` (a failing external stops the sequence) |
| `cmd1 \|\| cmd2` | `try { cmd1 } catch { cmd2 }` |
| `[[ -f x ]]` | `("x" \| path exists)` |
| `for f in *.log; do …; done` | `for f in (glob *.log) { … }` |
| `cmd \| head -5` on structured output | `cmd \| first 5` |
| exit code and stderr of a command | `^cmd \| complete` (record of `exit_code`, `stdout`, `stderr`) |

Builtins shadow coreutils of the same name (`find`, `ls`, `du`, `ps`, `sort`, `date`, `which`):
`find . -maxdepth 1` fails with an unknown-flag error, `^find . -maxdepth 1` works. Prefix an
external with `^` whenever its name might be a builtin.

`head` and `tail` are externals, so piping a table into them renders the bordered table as text
and slices the box-drawing lines. Use `first`, `last` and `get <i>` on structured data.

**Remote hosts.** `ssh host '<cmd>'` hands `<cmd>` to the remote login shell, which is Nushell.
Keep that form for one plain command; anything with pipes, quotes, regexes or several steps goes
through `ssh-bash <host> <<'EOF' … EOF`, which runs the heredoc under bash on the far end.

**Scripts in the repo.** `bin/*.nu` and `.github/scripts/*.nu` take arguments through
`def main [...]`, with subcommands as `def "main <sub>" [...]`. Give parameters types, write data
to stdout and diagnostics to stderr (`print -e`), and let a failing external stop the script
rather than swallowing it.

## Your own Bash tool work

Commands you run yourself through the Bash tool can be bash; a single external invocation is the
same in either shell. Within that work, Nushell earns its place for structured data.

**Use Nushell when:**
- Reading or writing structured data: JSON, YAML, TOML, CSV, XML, SQLite
- Transforming data between formats (e.g. JSON → CSV)
- Filtering, grouping or aggregating structured data
- Pipelines where bash quoting or escaping is error-prone
- You might otherwise write a Python script to understand a large JSON file

**Use native Unix tools when:**
- Simple text search: `grep`, `rg`
- Viewing the start or end of a text file: `head`, `tail`
- Line counting: `wc`
- Simple text transformation: `sed`, `awk`, `cut`
- File finding: `find`, `fd`
- Large files where streaming matters

Invoke it as `nu -c '<command>'`:

```bash
# Structured data
nu -c 'open data.json | get users | where active == true'

# Format conversion
nu -c 'open data.csv | to json' > data.json

# Filtering
nu -c 'open config.toml | get servers | select name port | where port > 8000'
```

### Common patterns

```bash
# JSON
nu -c 'open file.json | get items | length'
nu -c 'open file.json | select name email | to csv'

# CSV
nu -c 'open data.csv | where column > 100 | sort-by column'
nu -c 'open data.csv | group-by category | transpose'

# YAML/TOML
nu -c 'open config.yaml | get database.host'
nu -c 'open Cargo.toml | get dependencies'

# Nested JSON with awkward keys
nu -c 'open api-response.json | get data.items | each { |it| $it.name }'

# Arithmetic without bc
nu -c '(1024 * 1024 * 50) | into filesize'
```

### Combining with Unix tools

Pipe Unix tool output into Nushell for structured processing:

```bash
find . -name "*.log" -mtime -1 | nu -c 'lines | wrap path | to json'
podman ps --format json | nu -c 'from json | select Names Status'
```
