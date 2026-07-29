---
name: deploy-host
description: Use this skill when a change is ready to reach a real machine — deploying to atlas, pallas, hecate, janus, vulcan, or prometheus with deploy-rs, or verifying a deploy that already happened. Triggers include "deploy this", "push this to <host>", "roll this out", "did the deploy work", "the deploy rolled back", or any request that would run `nix run '.#deploy-rs'` / `nixos-rebuild switch` against a remote host. Encodes the rollback hazards, the Pi-hole DNS timing trap, and the per-host pre-flight checks that have each cost a real outage.
---

# Deploying a host

## Read this first: you do not initiate deploys

**Never run `deploy-rs`, `nixos-rebuild switch/boot/test`, or `darwin-rebuild switch` unless the
user explicitly asked you to deploy in this turn.** "The change is finished" is not authorization.
"It builds" is not authorization. Approval for one host is not approval for the next one.

The correct end state for almost every task is: change staged, evaluated, built, and a one-line
handoff telling the user the exact command to run. Stop there.

When you *have* been asked to deploy, everything below applies.

## Phase 1 — Pre-flight

Flake evaluation only sees git-tracked files, so stage first or you will deploy a config that
silently lacks your new files:

```nu
git add <new-or-changed-files>
```

Then confirm the thing you're about to ship actually builds. Never let the first build happen
inside a deploy — a build failure mid-activation is far worse than one at your terminal:

```nu
# one host
nixos-rebuild --flake .#<host> build

# all hosts in parallel (default: hyperion atlas vulcan janus pallas hecate prometheus)
nu bin/build-fan-out.nu
```

## Phase 2 — Know which hazard applies to this host

`flake.nix` sets `autoRollback = true` and `magicRollback = true` for every node, with no
`confirmTimeout`, so the default **30-second** confirmation window applies everywhere.

| host | system | notes |
|---|---|---|
| atlas | x86_64 | Podman/OCI, NVIDIA, ZFS data pool. LSI SAS2008 HBA has a history of mass-disconnect under thermal load — first suspect for any storage hang or mystery reboot, before you suspect your change |
| vulcan | x86_64 | Podman/OCI, Intel GPU, ZFS, forgejo runners. Deploying here can kill the runner executing your own CI |
| pallas | aarch64 | **Pi-hole.** `fastConnection = true`. See the DNS trap below |
| hecate | aarch64 | **Pi-hole**, keepalived peer with pallas. `fastConnection = true`. Same trap |
| janus | x86_64 | Cloud VPS, disko-managed. Netbird management plane — a netavark cross-network isolation change already broke this once |
| prometheus | aarch64 | `fastConnection = true`. Uses stock nixpkgs-unstable via `mkHost`, not the pinned channel — it can break independently of every other host, and it is absent from the host table in CLAUDE.md |

### The Pi-hole DNS trap (pallas, hecate)

Restarting the Pi-hole container drops LAN DNS. `magicRollback` needs to reach the host to confirm
activation within 30 seconds. If confirmation resolves the hostname through the very Pi-hole that
is mid-restart, it fails, and deploy-rs rolls back a **perfectly good activation**.

Deploy by IP so confirmation never needs DNS, and widen the window:

```nu
nix run '.#deploy-rs' -- --hostname <ip> --confirm-timeout 120 '.#pallas'
```

### The rollback-versus-external-state hazard

`autoRollback` restores the previous *system closure*. It does not restore anything activation
mutated outside that closure — ZFS datasets created or renamed, podman volumes, sops-rendered
files on disk. After a rollback the old spec is applied to the new live state, which is a
configuration no one ever tested.

Before deploying a change that creates/renames a dataset, changes a container's volume layout,
or moves a sops secret, decide what a rollback would do to that state. If the answer is "something
bad", deploy that change on its own, not bundled with unrelated work. On live hosts prefer moving
stale files into a quarantine directory over deleting them — a rollback that recreates a file you
deleted is a real scenario here.

## Phase 3 — Deploy

```nu
nix run '.#deploy-rs' '.#<host>'
```

Deploy one host at a time and verify before moving to the next, even when the change touches
several. Watch for the activation output and the confirmation — a deploy that reports success but
whose confirmation timed out has already rolled back.

## Phase 4 — Verify

Verification is not optional, and "the deploy command exited 0" is not verification.

```nu
# generation actually switched
ssh <host> 'nixos-rebuild list-generations | first 3'

# nothing failed to come up
ssh <host> 'systemctl --failed'

# Pi-hole hosts: confirm the DNS fallback survived (exits non-zero on failure)
nu bin/verify-dns-fallback.nu pallas
```

Remote commands run in **nushell** — every host's login shell is nu, so the string inside
`ssh host '...'` is nushell syntax even though `ssh` itself is a local invocation. Use
`first`/`last` rather than `head`/`tail` on structured output, and `ssh host 'bash -c "..."'`
if you genuinely need POSIX on the far end.

For anything more than a couple of checks, delegate to the `host-inspector` agent rather than
pulling journals into the main context.

### Things that look like deploy failures but aren't

- **A sops secret's value changed but the service still uses the old one.** Re-rendering the file
  does not restart `oci-containers`. The container keeps the stale environment until it is
  recreated — restart `podman-<service>` explicitly.
- **A container can't reach another container by name.** netavark 2.0 dropped cross-network
  hairpin. Check both are on the same podman network before blaming the deploy.
- **`short-name did not resolve`.** The v2 `registries.conf` default defines no
  `unqualified-search-registries`; the oci module sets it. This means that regressed, not that
  your change is wrong.

## Phase 5 — When it goes wrong

Report what actually happened, in this order: did activation run, did confirmation succeed, did
rollback fire, and what is the live state now. A rolled-back deploy plus mutated external state is
the dangerous combination — say so explicitly rather than reporting "deploy failed, retrying".

Do not retry a deploy that rolled back until you know why it rolled back. A second attempt at a
timing failure just rolls back again; a second attempt at a state failure compounds it.
