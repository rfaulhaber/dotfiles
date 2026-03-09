---
name: nushell
description: Use Nushell (nu) for shell operations involving structured data manipulation (JSON, YAML, TOML, CSV, XML), complex pipelines that would be error-prone in bash, or when the user explicitly requests Nushell. Prefer native Unix tools (grep, head, tail, awk, sed, find) for simple text searching, filtering, and file operations where memory efficiency matters.
---

# Nushell Shell Skill

## Decision Framework

**Use Nushell when:**
- Reading/writing structured data: JSON, YAML, TOML, CSV, XML, SQLite
- Transforming data between formats (e.g., JSON → CSV)
- Complex filtering, grouping, or aggregation on structured data
- Pipelines requiring type safety or where bash quoting/escaping is error-prone
- Working with nested data structures
- The user explicitly requests Nushell
- You might otherwise want to invoke a Python script to understand a large JSON file

**Use native Unix tools when:**
- Simple text search: `grep`, `rg`, `ag`
- Viewing file heads/tails: `head`, `tail`
- Line counting: `wc`
- Simple text transformation: `sed`, `awk`, `cut`
- File finding: `find`, `fd`
- Large file processing where streaming matters

## Nushell Invocation

Always invoke as: `nu -c '<command>'`

```bash
# Structured data
nu -c 'open data.json | get users | where active == true'

# Format conversion
nu -c 'open data.csv | to json' > data.json

# Complex filtering
nu -c 'open config.toml | get servers | select name port | where port > 8000'
```

## Common Patterns

### Structured Data Operations

```bash
# JSON manipulation
nu -c 'open file.json | get items | length'
nu -c 'open file.json | select name email | to csv'
nu -c '{"key": "value"} | to json' > out.json

# CSV operations
nu -c 'open data.csv | where column > 100 | sort-by column'
nu -c 'open data.csv | group-by category | transpose'

# YAML/TOML
nu -c 'open config.yaml | get database.host'
nu -c 'open Cargo.toml | get dependencies'
```

### When Bash Would Be Error-Prone

```bash
# Nested JSON with special characters - nu handles cleanly
nu -c 'open api-response.json | get data.items | each { |it| $it.name }'

# Safe string interpolation
nu -c 'let name = "test"; $"prefix-($name)-suffix"'

# Arithmetic without bc
nu -c '(1024 * 1024 * 50) | into filesize'
```

### Combining with Unix Tools

Pipe Unix tool output into Nushell for structured processing:

```bash
find . -name "*.log" -mtime -1 | nu -c 'lines | wrap path | to json'
docker ps --format json | nu -c 'lines | each { from json } | select Names Status'
```

## Anti-Patterns

Avoid Nushell for these—use native tools:

```bash
# Don't: nu -c 'open large.log | find "ERROR"'
# Do:
grep "ERROR" large.log

# Don't: nu -c 'open file.txt | first 10'
# Do:
head -10 file.txt

# Don't: nu -c 'ls | find ".rs"'
# Do:
find . -name "*.rs"
```
