---
name: nix
description: Use when working with Nix code, flakes, NixOS modules, home-manager, or nix-darwin configurations. Provides Nix language idioms, common patterns, and debugging strategies.
---

# Nix Development Skill

## Nix Language Essentials

### String Interpolation

```nix
# Use ${} inside double-quoted strings
"Hello ${name}"

# Multi-line strings use '' (two single quotes)
''
  line 1
  ${interpolated}
''

# Escape interpolation in multi-line strings with ''$
''
  literal ''${not_interpolated}
''
```

### Common Gotchas

- `//` is attrset merge (right takes precedence), NOT a comment
- `inherit (pkgs) git vim;` is shorthand for `git = pkgs.git; vim = pkgs.vim;`
- `with pkgs;` brings attrs into scope but is discouraged in module definitions (use `inherit` instead)
- Lists use spaces, not commas: `[ a b c ]`
- Function args use commas: `{ a, b, c }: ...`
- `builtins.trace` is `printf`-debugging for Nix evaluation
- `lib.debug.traceValSeq` pretty-prints a value and returns it

### Useful Evaluation Commands

```bash
# Evaluate an expression
nix eval .#<attr>

# Evaluate to JSON
nix eval --json .#<attr>

# Show flake outputs
nix flake show

# Build without installing
nix build .#<attr> --dry-run

# Enter a REPL with flake loaded
nix repl .#
```

## NixOS Module Pattern

```nix
{ config, lib, pkgs, ... }:
let
  cfg = config.modules.services.example;
in {
  options.modules.services.example = {
    enable = lib.mkEnableOption "example service";
    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "Port to listen on.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Only evaluated when enable = true
  };
}
```

### Key Module Functions

- `mkEnableOption` - creates a bool option defaulting to false
- `mkOption { type, default, description }` - general option
- `mkIf condition { ... }` - conditional config
- `mkMerge [ { ... } { ... } ]` - merge multiple config blocks
- `mkForce value` - override with high priority
- `mkDefault value` - set with low priority (easily overridden)
- `mkOrder n value` - explicit ordering (lower = earlier)

### Common Types

```nix
types.bool
types.str
types.int
types.port            # 0-65535
types.path
types.package
types.listOf types.str
types.attrsOf types.str
types.enum [ "a" "b" "c" ]
types.nullOr types.str
types.oneOf [ types.str types.int ]
types.submodule { options = { ... }; }
```

## Flake Patterns

### Running a Package Ephemerally

```bash
# Run a package without installing
nix run nixpkgs#<package> -- <args>

# Open a shell with packages available
nix shell nixpkgs#pkg1 nixpkgs#pkg2
```

### Debugging Build Failures

```bash
# Build with verbose output
nix build .#<attr> -L

# Show full trace on eval errors
nix build .#<attr> --show-trace

# Enter a build environment for debugging
nix develop .#<attr>

# Check what a derivation depends on
nix path-info -rsh .#<attr>
```

## Formatting

Always use `nix fmt` (which uses nixfmt in this repo) to format Nix files after editing them.
