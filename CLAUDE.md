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

# Available remote hosts: atlas, pallas, hecate, janus, vulcan
```

### Development Environment

```bash
# Enter dev shell (includes nil LSP, deploy-rs, sops-nix, dix, sops, rage)
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
nix build .#rpi3-installer              # Raspberry Pi 3 SD card image
nix build .#x86_64-installer            # x86_64 install ISO
nix build .#roc-rk3328-cc-bootloader    # Firefly ROC-RK3328-CC bootloader
```

Installer image modules live in `nix/images/`.

## Architecture

### Module System

The configuration uses a custom module system in `nix/modules/`. The purpose is to simplify or standardize underlying configuration. Modules are toggled on/off per-host via the `modules` attribute:

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
- `nix/modules/programs/` - User programs (emacs, git, nushell, sops, etc.)
- `nix/modules/linux/services/` - System services (zfs, ssh, nix-cache, netbird, keepalived, nfs, sudo-rs, etc.)
- `nix/modules/linux/services/desktop/` - Window managers and compositors
- `nix/modules/linux/services/hardware/` - Hardware-specific modules (nvidia, intel-gpu, bluetooth, zsa)
- `nix/modules/linux/oci/` - Podman-backed OCI container services
- `nix/modules/darwin/` - macOS-only modules
- `nix/modules/themes/` - Base16 theming

### Library Functions

Custom helpers in `nix/lib/` (see `nix/lib/nixos.nix` and `nix/lib/default.nix`):
- `mkNixOSHost` / `mkDarwinHost` / `mkRaspberryPiNixOSHost` - Create host configurations (auto-import `nix/modules` and wire home-manager)
- `mkOpt` / `mkOptDesc` - Option definition helpers
- `writeNushellScriptBin` - Wrap a Nushell script as a package
- `hostnameFromPath` - Derive hostname from `nix/hosts/<hostname>/configuration.nix`

Host builders pass `specialArgs` containing `inputs`, `lib`, `hostname`, `hostDir`, `isLinux`, and `isDarwin`.

### Host Configurations

| Host | System | Description |
|------|--------|-------------|
| hyperion | x86_64-linux | Primary desktop (Niri, NVIDIA, ZFS) |
| atlas | x86_64-linux | Media server (headless, NVIDIA, Podman/OCI) |
| vulcan | x86_64-linux | Secondary media + CI host (Intel GPU, ZFS, forgejo runners) |
| janus | x86_64-linux | Cloud VPS (disko-managed disk) |
| pallas | aarch64-linux | Raspberry Pi 4 server |
| hecate | aarch64-linux | Raspberry Pi 3 backup DNS / keepalived peer |
| eos | aarch64-darwin | macOS development machine |

Host directories under `nix/hosts/` without a corresponding `nixosConfigurations` entry in `flake.nix` (e.g., `helios`, `hestia`, `nexus`, `nike`) are retired/unused — do not assume they build.

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
- `zfs-manage.nu` - ZFS pool/dataset management
- `backup_zfs_dataset.nu` - ZFS dataset snapshot + send/recv backup
- `random-wallpaper.nu` - Unsplash wallpaper rotation
- `mullvad-config.nu` - VPN configuration
- `exec-emacs-project.nu` - Run a command in the context of a projectile project
- `tmp-downloads.nu` - Temporary downloads directory helper

### CI / Automation

- `.forgejo/workflows/` - Forgejo Actions workflows (e.g. `flake-update.yml`) executed by the self-hosted forgejo runners on `vulcan`.
- `.forgejo/scripts/` - Nushell scripts invoked from those workflows (`build-one-host.nu`, `update-inputs.nu`, `create-pr.nu`, etc.).
- `.github/workflows/` - GitHub-side mirror/CI; `CACHE-SETUP.md` documents the shared binary cache.

### OCI Container Services

Declarative container management via `modules.linux.oci`. Uses Podman with systemd integration.

```nix
modules.linux.oci = {
  enable = true;
  networks.default.enable = true;
  services.plex = {
    enable = true;
    baseDir = "/apps/plex";
    mediaDirs = { movies = "/mnt/media/movies"; tv = "/mnt/media/tv"; };
    gpu = "nvidia";   # or "intel"; omit for no GPU passthrough
  };
};
```

Available services (`nix/modules/linux/oci/`): `caddy`, `forgejo-runner`, `immich`, `immich-ml`, `jellyfin`, `miniflux`, `newt`, `pihole`, `plex`.

**Secrets handling:** Service modules accept `*File` options for secrets (e.g., `webPasswordFile`, `tokenFile`, `secretsFile`). These should be sops-nix secret or template paths. Environment files must be in `KEY=value` format — use `sops.templates` to render them.

**Naming convention:** Follows compose2nix patterns — networks are `${hostname}_${name}`, services wire to `podman-compose-${hostname}-root.target`.

**ZFS integration:** When `modules.linux.oci.zfs.enable = true` with a `pool` set, service modules that declare host paths auto-register datasets via `_managedPaths`, which are then materialized by `modules.services.zfs.datasets`. Services gain an ordering dependency on `zfs-manage-datasets.service`.

**Service helpers:** Service modules should build their systemd unit via `config.modules.linux.oci.lib.mkServiceConfig { networks, volumes, ... }` to inherit the root-target wiring, restart policy, and network/volume dependencies.

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
