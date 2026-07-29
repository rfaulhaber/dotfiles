---
name: bump-oci-images
description: Use this skill when updating the pinned container image digests in `nix/hosts/*/oci-images.json` — "bump the images on atlas", "update <service>'s container", "why is this service still on the old image", or reviewing the PR the weekly digest-refresh CI opened. Covers the local refresh script, how to read the resulting diff, and the deploy-time consequences of a digest change. Not for adding a new service — use draft-oci-service for that.
---

# Bumping OCI image digests

## The routine case is already automated

`.forgejo/workflows/oci-update.yml` runs weekly (Sundays 04:00 UTC), refreshes every digest,
commits to a branch, and opens a PR. It also accepts a `dry_run` input for manual dispatch.

So before doing anything: **is the user asking for something CI doesn't already do?** Legitimate
reasons to be here are an urgent single-service bump, a digest that needs to move now rather than
Sunday, reviewing the PR CI produced, or debugging why a bump didn't take effect. If the ask is
just "keep images current", the answer is that it already happens — say so.

## The script does the whole job

`.forgejo/scripts/update-oci-digests.nu` walks each host's `oci-images.json`, finds every leaf
record with both `version` and `digest`, resolves that entry's repository via `nix eval` against
the host config, fetches the live digest with `skopeo inspect`, rewrites the JSON in place, and
then validates by evaluating each affected host's toplevel.

```nu
# every host
nu .forgejo/scripts/update-oci-digests.nu

# one or more hosts (janus hecate atlas pallas vulcan)
nu .forgejo/scripts/update-oci-digests.nu atlas

# see what would change without writing anything
DRY_RUN=true nu .forgejo/scripts/update-oci-digests.nu atlas
```

It deliberately does not touch git — it leaves modified files in the working tree for review.
Do not reimplement any part of this inline. If it fails, fix the script.

Two things it already handles, so don't "fix" them again: it points
`CONTAINERS_REGISTRIES_CONF` at a minimal v2 file because skopeo ≥1.23 refuses to parse the legacy
v1 `registries.conf` that the NixOS containers module still emits, and it quotes each path segment
so dashed attribute names (`forgejo-runner`, `calibre-web-auto`) evaluate.

## Reading the diff

For each changed entry, ask whether that service *should* be on a floating tag at all.

- **Floating tag (`latest`) is fine** for services with a stable API surface where an unexpected
  version jump is recoverable.
- **Pin an explicit semver** for services that break on major bumps — Pi-hole is the standing
  example. A floating tag on one of those turns a routine digest refresh into an outage.

If a diff shows a service moving several major versions at once, that is the signal it should have
been pinned, not a reason to merge faster.

## After the bump: what a digest change actually costs

A digest change means the container gets recreated on the next deploy, which drags in every
consequence of that service restarting.

- **Pi-hole hosts (pallas, hecate) are the dangerous case.** Recreating Pi-hole drops LAN DNS
  inside deploy-rs's 30-second confirmation window, and a good activation gets rolled back. Deploy
  by IP with `--confirm-timeout 120`. See the `deploy-host` skill — do not deploy a Pi-hole digest
  bump without reading it.
- **Registry pulls need working DNS on the target host.** This is what made the Pi-hole bump
  deadlock: stopping the container left the host with no resolver, so the pull died with
  "no such host" mid-activation. Verify with `nu bin/verify-dns-fallback.nu` before and after.
- **A sops-rendered environment file does not reload on its own.** If the bump coincides with a
  changed secret, the recreated container picks up the new file, but a secret changed *without* a
  digest change does not — restart `podman-<service>` explicitly.

## Verifying it took

```nu
# the digest the host is actually running
ssh <host> 'podman inspect <container> | get 0.ImageDigest'
```

Compare against `oci-images.json`. A mismatch means the container was never recreated — the deploy
updated the spec without restarting the unit.

Remote commands run in nushell, and `podman inspect` output is structured — use `get`, not `jq`
pipelines, and `first`/`last` rather than `head`/`tail`.
