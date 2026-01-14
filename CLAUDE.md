# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal dotfiles repository for NixOS and macOS systems using Nix flakes. Manages multiple machines with reproducible, modular configurations. Architecture heavily inspired by [hlissner's dotfiles](https://github.com/hlissner/dotfiles).

## Common Commands

### Building System Configurations

```bash
# Build a NixOS configuration (dry-run)
nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel

# Apply configuration on current machine
sudo nixos-rebuild switch --flake .#<hostname>

# Apply Darwin configuration on macOS
darwin-rebuild switch --flake .#eos
```

### Remote Deployment

```bash
# Deploy to a remote host using deploy-rs
nix run '.#deploy-rs' '.#<hostname>'

# Available remote hosts: atlas, pallas, janus
```

### Development Environment

```bash
# Enter dev shell (includes nil LSP, deploy-rs, sops, rage)
nix develop

# Format Nix files
nix fmt
```

### Creating New Projects

```bash
nix flake init -t .#rust        # Rust project template
nix flake init -t .#emacs-lisp  # Emacs Lisp template
```

### Building Installer Images

```bash
nix build .#arm-installer-generic      # Raspberry Pi SD card image
nix build .#x86_64-installer-generic   # x86_64 install ISO
```

## Architecture

### Module System

The configuration uses a custom module system in `nix/modules/`. Modules are toggled on/off per-host via the `modules` attribute:

```nix
# In a host configuration (e.g., nix/hosts/hyperion/configuration.nix)
modules = {
  programs.emacs.enable = true;
  services.docker.enable = true;
  desktop.enable = true;
  desktop.environment.niri.enable = true;
};
```

Module locations:
- `nix/modules/programs/` - User programs (emacs, git, nushell, etc.)
- `nix/modules/linux/services/` - System services (docker, zfs, ssh, desktop environments)
- `nix/modules/linux/services/desktop/` - Window managers and compositors

### Library Functions

Custom helpers in `nix/lib/` provide:
- `mkNixOSHost` / `mkDarwinHost` / `mkRaspberryPiNixOSHost` - Create host configurations
- `mapModules` - Auto-import Nix files as modules
- `mkOpt` / `mkOptDesc` - Option definition helpers

### Host Configurations

| Host | System | Description |
|------|--------|-------------|
| hyperion | x86_64-linux | Primary desktop (Niri, NVIDIA, ZFS) |
| atlas | x86_64-linux | Media server (headless, NVIDIA, Docker) |
| janus | x86_64-linux | Cloud VPS (disko-managed disk) |
| pallas | aarch64-linux | Raspberry Pi 4 server |
| eos | aarch64-darwin | macOS development machine |

### Secrets Management

Uses SOPS with age encryption. Keys configured in `.sops.yaml`. Access secrets in modules via:
```nix
config.sops.secrets.<name>.path
```

### Theme System

Base16 theming via `modules.themes.active`. Generates JSON/SCSS globals consumed by applications. Set per-host:
```nix
modules.themes.active = "tokyo-night-dark";
```

### Configuration Files

Application configs live in `/config/` and are symlinked to `~/.config/`. Notable:
- `/doom.d/` - Doom Emacs configuration
- `/config/nushell/` - Nushell with host-specific configs in `hosts/`
- `/config/niri/`, `/config/hypr/`, `/config/sway/` - Wayland compositor configs

### Custom Scripts

Nushell scripts in `/bin/` for system tasks:
- `zfs-manage.nu` - ZFS pool management
- `random-wallpaper.nu` - Unsplash wallpaper rotation
- `mullvad-config.nu` - VPN configuration

### OCI Container Services

Declarative container management via `modules.linux.oci`. Uses Podman with systemd integration.

```nix
modules.linux.oci = {
  enable = true;
  networks.default.enable = true;
  services.plex = {
    enable = true;
    baseDir = "/data/apps/plex";
    mediaDirs = { movies = "/data/movies"; tv = "/data/tv"; };
    useNvidia = true;
  };
};
```

Module locations: `nix/modules/linux/oci/`

**Secrets handling:** Service modules accept `*File` options for secrets (e.g., `webPasswordFile`). These should be sops-nix secret paths. Environment files must be in `KEY=value` format - use `sops.templates` if needed.

**Naming convention:** Follows compose2nix patterns - networks are `${hostname}_${name}`, services wire to `podman-compose-${hostname}-root.target`.

## Key Patterns

**Module option pattern:**
```nix
{config, lib, ...}: let
  cfg = config.modules.services.example;
in {
  options.modules.services.example = {
    enable = lib.mkEnableOption "example service";
  };
  config = lib.mkIf cfg.enable {
    # configuration here
  };
}
```

**Desktop environment assertion:** When `modules.desktop.enable = true`, exactly one desktop environment must be enabled (niri, hyprland, sway, etc.).

**Platform flags:** Use `isLinux`, `isDarwin`, `isWayland`, `isX11` for platform-specific logic within modules.
