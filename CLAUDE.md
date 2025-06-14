# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## CRITICAL RESTRICTIONS

**NEVER run any of the following commands without explicit user approval:**
- Any `nix` commands that build or change system state (nix build, nix develop, nixos-rebuild, etc.)
- Any `git` commands (git add, git commit, git push, etc.)
- Any `doom` commands (doom sync, doom upgrade, doom doctor, etc.)
- Any deployment commands (deploy-rs, etc.)

**EXCEPTION:** `nix fmt` is safe to run and SHOULD be run after making substantial Nix code changes.

These commands can make system-level changes or modify the repository state. Always ask the user before running any potentially destructive operations.

## Repository Overview

This is a comprehensive dotfiles repository managing both system-level (NixOS) and user-level (Doom Emacs + shell) configurations across multiple machines. The repository supports both Linux and macOS environments with a modular, host-specific configuration system.

## Key Commands

### Nix/NixOS Operations
```bash
# Format Nix code
nix fmt

# Build specific NixOS configuration
nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel

# Apply configuration locally (NixOS)
nixos-rebuild switch --flake .#<hostname>

# Deploy to remote hosts
nix run '.#deploy-rs' '.#<hostname>'

# Generate installers
nix build .#arm-roc-installer          # ARM installer for Renegade ROC
nix build .#x86_64-installer-generic   # Generic x86_64 installer

# Development environment
nix develop
```

### Available Host Targets
- **hyperion**: x86_64-linux (main desktop)
- **atlas**: x86_64-linux (server)
- **janus**: x86_64-linux (VPS)
- **pallas**: aarch64-linux (ARM server)
- **nike**: aarch64-linux (ARM server)

### Doom Emacs
```bash
# From doom.d directory
doom sync    # Sync packages after changing init.el or packages.el
doom upgrade # Update Doom and packages
doom doctor  # Check for configuration issues
```

## Architecture Overview

### NixOS Module System (`nix/`)

**Core Structure:**
- `nix/modules/` - Modular system components organized by category
- `nix/hosts/` - Host-specific configurations that compose modules
- `nix/lib/` - Utility functions and module creation helpers
- `flake.nix` - Main entry point defining all system configurations

**Module Categories:**
- `desktop/` - Desktop environments (Hyprland, Niri, Sway, GNOME, etc.)
- `programs/` - Application configurations (Emacs, Git, Nushell, etc.)
- `services/` - System services (Docker, ZFS, SSH, etc.)
- `hardware/` - Hardware-specific configurations (Nvidia, ZSA keyboards, etc.)
- `themes/` - Centralized theming system using Base16 color schemes

**Key Patterns:**
- Each module follows `modules.<category>.<name>.enable` pattern
- Host configurations are declarative compositions of enabled modules
- Automatic module discovery and loading via `mapModules`
- Consistent option patterns using `mkOpt`, `mkEnableOption`
- Integration between system and home-manager configurations

### Doom Emacs Configuration (`doom.d/`)

**Core Architecture:**
- `init.el` - Module declarations using `doom!` macro
- `config.el` - Personal configuration with host-specific loading
- `packages.el` - Additional package declarations
- `hosts/` - Host-specific configurations (font sizes, work flags, etc.)
- `self/` - Custom utility functions and work-related code

**Host-Specific Loading:**
- Dynamic host detection via `self/system-name` and `self/system-type`
- Conditional loading: `(load! (format "./hosts/%s" self/system-name))`
- Work computer detection with `config/work-computer-p`

**Key Features:**
- Extensive Org-mode integration (roam, journal, agenda, publish)
- LSP support via eglot for multiple languages
- Custom evil ex commands and key bindings
- Modular configuration with clear separation of concerns

### Configuration Files (`config/`)

Application-specific configuration files organized by tool:
- `nushell/` - Shell configuration with host-specific files
- `wezterm/` - Terminal emulator configuration per host
- `hypr/`, `sway/`, `niri/` - Wayland compositor configurations
- `awesome/` - AwesomeWM configuration in Fennel

## Development Patterns

### NixOS Module Development
- Use `mkEnableOption` for boolean toggles
- Follow the pattern: `config = mkIf cfg.enable { ... }`
- Place shared configuration in `nix/modules/`
- Host-specific overrides go in `nix/hosts/<hostname>/`

### Adding New Hosts
1. Create `nix/hosts/<hostname>/` directory
2. Add `configuration.nix` and `hardware-configuration.nix`
3. Register in `flake.nix` nixosConfigurations
4. Add corresponding Emacs host file in `doom.d/hosts/`
5. Add shell configuration in `config/nushell/hosts/`

### Theme System
- Themes defined in `nix/modules/themes/`
- Set via `modules.themes.active = "theme-name"`
- Exports theme data as JSON/SCSS for non-declarative applications
- Consistent color schemes across all applications

## Important Notes

- The flake structure supports both x86_64-linux and aarch64-linux architectures
- Remote deployment is handled via deploy-rs for headless servers
- Secrets management uses agenix (age encryption)
- All configurations are reproducible and version-controlled
- Host detection enables the same configuration to work across different machines
- The configuration emphasizes modularity and reusability across different host types