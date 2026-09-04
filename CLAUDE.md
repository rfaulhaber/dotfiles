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

# Available remote hosts: atlas, pallas, hecate, janus, vulcan, prometheus
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
nix build .#rpi5-installer              # Raspberry Pi 5 SD card image
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
- `nix/modules/hardware/` - Board-specific modules shared between a host and an image. Unlike every other dir here these are **not** in `nix/modules/default.nix`'s import list — they set unconditional config and need vendor `specialArgs`, so consumers import them by relative path (as images already do for `../modules/ssh/keys.nix`).

### Library Functions

Custom helpers in `nix/lib/` (see `nix/lib/nixos.nix` and `nix/lib/default.nix`):
- `mkNixOSHost` / `mkDarwinHost` - Create host configurations (auto-import `nix/modules` and wire home-manager)
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
| prometheus | aarch64-linux | Raspberry Pi 5, vendor kernel, forgejo/codeberg CI runner |
| eos | aarch64-darwin | macOS development machine |

Host directories under `nix/hosts/` without a corresponding `nixosConfigurations` entry in `flake.nix` are retired/unused — do not assume they build.

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
- `/generated/` - Rendered ghostty/nushell configs for hosts NOT managed by Nix (external consumers copy them; nothing in-repo reads them — see its README). Refresh with `nix build .#generated-configs`.
- `/doom.d/` - Doom Emacs configuration
- `/config/nushell/` - Nushell with host-specific configs in `hosts/`
- Niri (the active Wayland compositor) is configured inline in `nix/modules/linux/services/desktop/environment/niri/`, not under `/config/`

### Custom Scripts

Nushell scripts in `/bin/` for system tasks:
- `zfs-manage.nu` - ZFS pool/dataset management
- `extract-embedded-subs.nu` - Extract embedded subtitle tracks to sidecar files to avoid Jellyfin's full-container NFS demux
- `random-wallpaper.nu` - Unsplash wallpaper rotation
- `exec-emacs-project.nu` - Run a command in the context of a projectile project
- `open-zellij-workspace.nu` - Pick a project via `noctalia dmenu` and open it in a zellij session
- `launch-executable.nu` - Pick an executable from PATH via `noctalia dmenu` and run it
- `bandcamp-import.nu` - Import Bandcamp purchases into the atlas music library, with Lidarr catalog registration
- `build-fan-out.nu` - Build every host's toplevel in parallel
- `nix-lines-history.nu` - Emit CSV of file line counts sampled across git history
- `vpn.nu` - Switch NetworkManager VPN profiles (`status`, `up`, `down`, `toggle [name]`); packaged as `vpn` by `modules.services.airvpn`, default profile from `VPN_PROFILE`
- `update-oci-digests.nu` - Refresh pinned container digests in `nix/hosts/*/oci-images.json`, and warn when an explicitly-versioned entry has a newer upstream release on the same tag line (a digest refresh can't move those). Dual-purpose: run by hand for an urgent bump, and invoked by `oci-update.yml` weekly. Needs `skopeo`.

### CI / Automation

- `.github/workflows/` - GitHub Actions CI. `build-and-cache.yml` builds every host's toplevel on the self-hosted runners (vulcan for x86_64, prometheus for aarch64) after each push to main; closures land in the runner hosts' stores, which harmonia serves as binary caches. `eval.yml` evaluates every host's toplevel on GitHub-hosted runners for PRs (fork-safe, no LAN access). `flake-update.yml` bumps nixpkgs nightly, fans the bump out across the 7-host build matrix, and opens a PR with a per-host result table. `oci-update.yml` refreshes every pinned container digest weekly and opens a PR.
- `.github/hosts.json` - Single source of truth for which hosts CI covers and which runner label builds them.
- `.github/scripts/` - Nushell helpers invoked from the workflows.
- Self-hosted runners are managed natively by `modules.services.github-runner` (`nix/modules/linux/services/github-runner/`), registered per-repo with an ephemeral lifecycle.
- The runners are `allowed-users` but deliberately **not** `trusted-users` on the host nix daemon. Two consequences bind every workflow: a job-supplied `NIX_CONFIG` with `extra-substituters`/`extra-trusted-public-keys` is silently ignored (substituters come from the host's own `nix.settings`), and `nix copy --no-check-sigs` cannot work. Build straight into the host store instead.
- Their PATH carries only `nix`, `nu`, `git`, `bash` and core utilities — reach anything else (`skopeo`, `jq`, `gh`, `curl`) via `nix shell nixpkgs#<pkg> --command ...`.
- Any `run:` step containing a pipe needs an explicit `shell: bash`. Without it steps execute under `sh -e`, which has no `pipefail`, so a failing command upstream of a pipe reports success.
- `.forgejo/workflows/` + `.forgejo/scripts/` - Superseded Forgejo Actions from the Codeberg era, kept until the ported GitHub workflows have each run green once. Nothing executes them today: the runners that claimed them were removed, and the `git.home.lan` mirror has Actions disabled.

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
