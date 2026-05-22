---
name: draft-oci-service
description: Use this skill when the user asks to add, draft, scaffold, or "stub out" a new OCI/podman containerized service for atlas (or any host using `modules.linux.oci.services.*`). Triggers include "add a new service", "set up <service> on atlas", "create an OCI module for X", "wrap <X> in nix", or any request to integrate a new docker-compose-style service into the dotfiles repo. The skill encodes hard-won conventions from the atlas-oci migration — sops patterns, postgres gotchas, encrypted dataset handling, gluetun integration, ZFS layout, and the bugs that bit during the original cutover.
---

# Drafting an OCI service module for atlas

You are scaffolding a new `nix/modules/linux/oci/<service>.nix` plus the wiring in `nix/hosts/atlas/oci.nix` (or whichever host applies). The patterns here come from a multi-week migration that surfaced a long list of subtle bugs — follow them or you'll re-introduce those bugs.

**Don't draft code in Phase 1.** Get the service model straight first. Half the bugs in the original migration came from drafting before understanding.

---

## Phase 1 — Interrogate the service

Batch these into one or two messages. Skip what the user already told you.

### Container basics

- **Image**: where does it live? `ghcr.io/...`, `docker.io/<org>/<name>`, `lscr.io/linuxserver/...`. Confirm the canonical path — some images aren't where you'd guess. (Example bug from history: the module defaulted to `valkey:8-bookworm`, which fails because the canonical name is `valkey/valkey:8-bookworm` under the `valkey` org, not `docker.io/library/valkey`.)
- **Image style**: linuxserver.io (uses `PUID`/`PGID` env, runs as that user) vs. official-style (runs as a dedicated container user) vs. runs-as-root. Determines whether `chown` is needed on baseDir, and what `--user` flag (if any) to pass.
- **Stateful directories**: what does the service write to? Just `/config`? `/config` + `/data`? Nested? Look up the image's docs/README — don't guess.
- **Database**: SQLite (no special handling), Postgres (separate dataset with `recordsize=8K` + sops-rendered `DATABASE_URL`), LiteDB (treat like SQLite), none.
- **External media bind-mounts**: does it need read access to `/data/movies`, `/data/music`, etc.? These are *not* registered as managed datasets — pass them as opaque mount strings.

### Networking

- **Gluetun-routed?** If the service must egress through the VPN (anything that talks to trackers, Soulseek, Usenet providers, or any service whose IP is identifying), it joins gluetun's netns via `useGluetun = true`. Its host port mappings get aggregated into `_gluetunPorts` (gluetun publishes them since the joined container can't bind host ports itself).
- **Own network?** Postgres-using services historically get their own network (immich, miniflux, forgejo) so the postgres sidecar isn't accessible from the default network. Single-container services use `["default"]`.
- **Externally tunneled via newt/Pangolin?** If the service should be reachable from the public Pangolin tunnel, its network must be in newt's `networks` list.
- **Don't forward default port numbers**  Many services with container setups tell you to use a specific port. Atlas in particular has a lot of used ports already. You should check to see if the port is already in use in the configuration, and if it is, generate a random port number.

### Auth & secrets

- **Admin password / API token / DB password?** Each goes into sops at `<svc>/<key>` and is rendered into an env file via `sops.templates."<svc>-env"`.
- **OIDC?** If the service supports SSO via `auth.3679.space` (PocketID), gate `oidc.enable` on a sub-toggle and declare `<svc>/oidc-client-id` + `<svc>/oidc-client-secret` *only when enabled*.
- **Identifying info beyond passwords?** Per project policy: usernames, VPN IPs, account IDs, license keys all go through sops, not plain Nix.

### Inter-service deps

- Hard `dependsOn` (other podman containers): list explicitly.
- Soft "would be useful" deps: usually skip — `Restart=always` will recover.

### Health check

- Does the image expose one (`HEALTHCHECK` in its Dockerfile, or a known endpoint)? Add `--health-cmd`, `--health-interval`, `--health-start-period`. Postgres: `pg_isready -d <db> -U <user>`. HTTP services: `curl -fsS http://localhost:<port>/health || exit 1`.

---

## Phase 2 — Reality check

Before writing nix, verify:

1. **Image name and tag actually exist.** `podman search <image>` or check Docker Hub directly. If the user names a tag like `latest`, also note the current version it resolves to (so the comment in `image = mkOption { default = ... }` is informative).
2. **Look at the legacy docker-compose** if one exists. The user's compose lives at `~/Projects/infra/atlas/` (workstation, not on atlas itself). Read it for: env vars, volume layout, port mappings, custom `PGDATA`, weird `cap_add`, `--user`. Things that look load-bearing in compose are usually load-bearing in the new module too. (Example bug from history: miniflux's legacy data was nested at `<base>/18/docker/PG_VERSION` because compose set `PGDATA=/var/lib/postgresql/data/18/docker`. Without setting `pgdata` in the new module, postgres tried to `initdb` over existing data and crashed.)
3. **For postgres-using services migrating from existing data: capture `PG_VERSION` exactly.** The on-disk data is locked to its major version. The new module's `postgres.image` *must* match. Downgrade requires `pg_dumpall`/restore — there is no shortcut.
4. **Check whether the image runs as non-root.** If yes and it tries to bind a port `<1024`, you have to either add `cap_add: NET_BIND_SERVICE` *or* configure the service to listen on a high port. (Example bug from history: filebrowser-quantum runs as nobody and binds 80 by default — failed with "permission denied" until the internal port was changed to 6572.)

Don't ask the user about these details speculatively — go look. If the lookup turns up something surprising, *then* surface it.

---

## Phase 3 — Draft the module

### File location

`nix/modules/linux/oci/<service>.nix` — also add it to the `imports = [ ... ]` list in `nix/modules/linux/oci/default.nix`.

### Standard module skeleton

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.<service>;
  ociLib = config.modules.linux.oci.lib;
  # If the service has its own network:
  networkName = "<service>";
in {
  options.modules.linux.oci.services.<service> = {
    enable = mkEnableOption "<one-line description>";

    image = mkOption {
      description = "<service> container image.";
      type = types.str;
      default = "<canonical/image>:<tag>";
    };

    baseDir = mkOption {
      description = "Single state directory for <service>, mounted at <container-path>.";
      type = types.str;
      example = "/data/apps/<service>";
    };

    # ... port, networks, dependsOn, extraEnv, timezone, user as needed ...

    configProperties = mkOption {
      description = "ZFS properties applied to baseDir. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};   # or {recordsize = "8K";} for postgres-only
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets."<service>/<key>" = {};

    sops.templates."<service>-env".content = ''
      KEY=${config.sops.placeholder."<service>/<key>"}
    '';

    virtualisation.oci-containers.containers.<service> = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment = { ... };
      environmentFiles = [config.sops.templates."<service>-env".path];
      volumes = [
        "${cfg.baseDir}:<container-path>"
        # plus media mounts, NOT registered as managed paths
      ];
      ports = ["${toString cfg.port}:<container-port>"];
      extraOptions =
        ["--network-alias=<service>"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
      log-driver = "journald";
    };

    systemd.services."podman-<service>" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };
  };
}
```

### For *arr-style services (linuxserver.io images, single config dir)

Use the `mkArrService` helper from `ociLib`. Cuts boilerplate significantly. See `nix/modules/linux/oci/radarr.nix` (etc.) for the shape. It handles PUID/PGID, gluetun integration via `useGluetun`, the `--network=container:gluetun` option, and `_gluetunPorts` aggregation automatically.

### For postgres-using services

- **Two datasets** under a `mountpoint=none` parent: `data/apps/<svc>/files` (or whatever the app-data is) at `recordsize=64K`, `data/apps/<svc>/db` at `recordsize=8K`. Register both via `_managedPaths` with the parent at `mountpoint = "none"`.
- **Render the full `DATABASE_URL` in the sops template**, not in the inline `environment` block. **Critical**: podman does *not* expand shell-style `$VAR` references in env values (docker-compose did, which is why the legacy worked and the first nix port broke). The `sops.placeholder` substitution happens at template-render time, before the env reaches the container.
- **Add a `pgdata` option** that defaults to `/var/lib/postgresql/data` — overridable for legacy data layouts where the actual `PG_VERSION` is nested deeper.
- **Health check** the postgres container: `--health-cmd=pg_isready -d <db> -U <user> --health-interval=10s --health-start-period=30s`.
- **Pin the postgres image to the exact major version** matching on-disk data. Downgrades require `pg_dumpall` first.

Example template for postgres + app:

```nix
sops.templates."<svc>-db-env".content = ''
  POSTGRES_PASSWORD=${config.sops.placeholder."<svc>/db-password"}
  DATABASE_URL=postgres://${cfg.postgres.user}:${config.sops.placeholder."<svc>/db-password"}@<svc>_db/${cfg.postgres.database}?sslmode=disable
'';
```

Both containers consume this env file. The postgres sidecar ignores `DATABASE_URL`; the app uses it directly (no shell expansion needed).

### For services with structured config files (YAML/JSON)

If the upstream supports a config file (`config.yml`, `config.json`, etc.), expose its fields as nix options and render via a sops template if the config must contain secrets, or `pkgs.writeText` if not. Build the structure as a nix attrset, then `builtins.toJSON` it (JSON is valid YAML — works for YAML consumers without an extra dep). Bind-mount the rendered file *on top of* the baseDir bind-mount: `"${config.sops.templates."<svc>-config".path}:<container-config-path>:ro"`. Set `mode = "0444"` on the sops template so the non-root container user can read it through `/run/secrets/`. See `nix/modules/linux/oci/filebrowser.nix` for the pattern. Not every field in the config necessarily has to be exposed as a module option: sensible defaults can be assumed. The options that should always be exposed are things that might vary between host machines or depend on the nix configuration. This largely depends on the service you're implementing. The user wishes to avoid boilerplate configuration in the codebase. Read the configuration details of the service to understand what should be surfaced versus what can be defaulted.

### For services with encrypted datasets

If the host has a ZFS-encrypted dataset backing the service:

- Add `<volume>Encryption.{enable, keyFile}` options to the module.
- When enabled, declare the sops binary key: `sops.secrets."<svc>/zfs-key" = { format = "binary"; sopsFile = cfg.<volume>Encryption.keyFile; };`.
- Set `canmount = "noauto"` on the encrypted dataset's properties + the encryption properties (`encryption = "aes-256-gcm"; keyformat = "raw"; keylocation = "file://${config.sops.secrets."<svc>/zfs-key".path}";`).
- Register an entry in `modules.services.zfs.encryptedDatasets.<name> = { dataset = ...; keyFile = ...; consumers = ["podman-<svc>.service"]; };`. The consumer relationship makes the podman service wait for the dedicated unlock unit.
- See `nix/modules/linux/oci/immich.nix:filesEncryption` for the canonical pattern.

### For gluetun-routed services (VPN egress required)

- Set `useGluetun = true` (if using `mkArrService`) or manually pass `--network=container:gluetun` in `extraOptions` and **omit** `--network=...` for any other network (you can only join one netns).
- Aggregate any host ports the service exposes into `_gluetunPorts` so gluetun publishes them — the container can't bind host ports itself once joined to gluetun's netns.
- `dependsOn = ["gluetun"];` for ordering.

### Cross-cutting standards

- `timezone = mkOption { type = types.str; default = "America/New_York"; };` for atlas. Different default for other hosts.
- `networks = mkOption { type = types.listOf types.str; default = ["default"]; };` unless this service gets its own.
- `dependsOn = mkOption { type = types.listOf types.str; default = []; };` always present, lets host config add ordering.
- `extraEnv = mkOption { type = types.attrsOf types.str; default = {}; };` always present.
- `log-driver = "journald";` always — keeps logs in the host journal where they're searchable.

---

## Phase 4 — Wire into the host

### In `nix/hosts/atlas/oci.nix`

Add the service config in the appropriate wave block (mirrors `MIGRATION.org`'s organization):

- **Wave 1** (`gluetun-routed`): downloaders, *arrs, anything VPN-bound
- **Wave 2** (non-gluetun): databases, file browsers, dashboards, etc.

Match the alphabetical-ish ordering within waves where it exists.

### Sops secrets

For every `sops.secrets."<svc>/<key>" = {}` the module declares, add a corresponding entry to `nix/hosts/atlas/secrets.yaml`. The slash in the nix key is the YAML hierarchy separator:

```yaml
<svc>:
    <key>: <value-to-be-encrypted>
```

User runs `sops nix/hosts/atlas/secrets.yaml` to edit. Existing per-service blocks group cleanly — add to them rather than creating a new top-level entry if the service already has secrets.

For binary secrets (raw ZFS keys), use a separate file at `nix/hosts/atlas/secrets/<name>` and set `format = "binary"; sopsFile = ./secrets/<name>;` in the module. The `.sops.yaml` rule `.*/atlas/secrets/.*$` already covers them.

### MIGRATION.org

Only relevant if cutting over from existing data on disk. Add a per-service section under `* Per-service steps` with subsections:

- `*** Pre-cutover` — anything that has to happen before `nixos-rebuild switch` (e.g., encrypting an existing key into sops, stopping legacy containers cleanly with `docker-compose down` for postgres).
- `*** Stop & relocate` — the on-disk shuffle. If using the migration script, reference `migrate-datasets.nu`; otherwise inline the commands.
- `*** Secrets` — what each sops key does, where the legacy value lives.
- `*** Verify after switch` — `systemctl status`, `curl` checks, mountpoint/recordsize verification.
- `*** Post-cutover` (optional) — cleanup of legacy paths/keys.

For services starting fresh (no legacy data), MIGRATION.org may not need an entry at all.

### Migration script (`nix/hosts/atlas/migrate-datasets.nu`)

Only if cutting over from existing data:

- **Single-volume from `/docker/config/<svc>`**: `migrate` op, source = legacy path, dataset = new dataset name, dst = new mountpoint, chown if linuxserver-style.
- **Single-volume from a dataset with wrong mountpoint**: `set-mountpoint` op (no data movement, just retargets the mount).
- **Collapsing parent + only child**: `promote-to-parent` op (atomic 3-step rename via `<parent>_collapse_tmp`).
- **Collapsing parent + multi-child** (one survives): `promote-to-parent` op (mount-parent-first + cross-fs mv into parent).
- **Empty residual to remove**: `destroy-if-empty` op (allows empty snapshots, uses `zfs destroy -r`).

Always do a `--dry-run` pass first.

### newt (Pangolin tunnel)

If the service should be externally reachable via Pangolin, add the service's network to newt's `networks` list in `nix/hosts/atlas/oci.nix`. Newt joins each listed network so it can reach the service by alias.

---

## Phase 5 — Verify

Run these locally before declaring done:

```bash
# Syntax check the changed nix files
for f in <list of changed files>; do
  nix-instantiate --parse "$f" > /dev/null && echo "$f: OK"
done

# Full atlas evaluation (if any sops secret files don't exist yet,
# generate placeholder binaries first, git add --intent-to-add them,
# then clean up after — see the verification dance from the migration
# session). For OIDC/password sops secrets that ARE in secrets.yaml,
# no placeholders needed.
nix eval .#nixosConfigurations.atlas.config.system.build.toplevel.drvPath
```

Inspect the rendered output for any service with sops templates:

```bash
nix eval --raw '.#nixosConfigurations.atlas.config.sops.templates."<svc>-env".content'
nix eval --raw '.#nixosConfigurations.atlas.config.sops.templates."<svc>-config".content'  # if structured config
```

You should see `<SOPS:hash:PLACEHOLDER>` markers wherever placeholders are referenced — sops-nix substitutes those at activation time.

---

## Reference: gotchas the migration surfaced

These all bit during the cutover; encoding so they don't bite again:

1. **Image names matter.** `docker.io/library/<name>` is *not* the same as `docker.io/<org>/<name>`. Always verify against the canonical registry path (valkey, immich's pgvector, etc.).
2. **Postgres data versions are sticky.** The on-disk `PG_VERSION` file dictates the required postgres image major version. Mismatch → postgres refuses to start. There is no fast in-place upgrade or downgrade path.
3. **Podman doesn't expand `$VAR` in env values.** docker-compose did. If a value needs substitution (database URL with embedded password, etc.), do the substitution in a sops template, not the env block.
4. **Postgres `POSTGRES_PASSWORD` env is initdb-only.** After the cluster exists, the env var is informational. Auth uses what's stored in `pg_authid`. For migrated data, sops *must* match the legacy password (or you `ALTER USER` to align).
5. **Container users matter for low ports.** Many modern containers run as non-root and can't bind `<1024`. Either change the internal port or add `--cap-add=NET_BIND_SERVICE`.
6. **`mountpoint=none` datasets shouldn't be auto-mounted.** `bin/zfs-manage.nu` skips them, but if you write any custom mount logic, replicate that check.
7. **`canmount=noauto` doesn't unmount currently-mounted datasets.** It only affects whether the dataset auto-mounts on `zfs mount -a` / boot. Useful for opting encrypted datasets out of the early `zfs-mount.service` so a dedicated unlock unit can mount them later.
8. **`keylocation` is mutable.** `encryption` and `keyformat` are not. So a dataset's key file path can be changed in place; the cipher/format can't.
9. **Sops secret keys with `/` map to YAML nesting.** `<svc>/<key>` in nix → `<svc>:\n  <key>:` in `secrets.yaml`. Same for `.env`-format sops files (where it's flat — the slash becomes part of the key name).
10. **Sops templates render at activation time, not build time.** The build-time content has placeholder markers; the runtime file at `/run/secrets/<name>` has real values. `nix eval` shows the placeholders.
11. **ZFS-encrypted datasets need their unlock unit ordered against `sops-install-secrets`. There is no `sops-install-secrets.service`.** sops-nix runs as an activation script. The right ordering is `After=zfs-import.target` (which is reached after activation, so sops keys are already rendered). Don't add `Requires=sops-install-secrets.service`.
12. **deploy-rs's magicRollback timeout is too short for first-time container deploys.** Pass `--confirm-timeout 600` for cutover deploys, or use `nixos-rebuild switch --flake .#<host> --target-host <host>` directly.
13. **Activation can leave ryan's user-level dbus-broker in a broken state** if it gets restarted mid-activation. Benign — recovers on next user@1000 start. Mitigate per-host with `systemd.user.services.dbus-broker.unitConfig.X-RestartIfChanged = false;` if it bothers you.

---

## Reference: atlas-specific conventions (won't apply to other hosts as-is)

- ZFS pool: `data` (so all OCI datasets are `data/apps/<svc>/...`).
- baseDir convention: `/data/apps/<svc>` (single dataset directly mounted to the container's config path).
- Shared media at `/data/movies`, `/data/music`, `/data/books`, `/data/tv` — passed as bind-mount sources, *not* registered as managed paths.
- User-content datasets at `/data/sync`, `/data/org`, `/data/photos`, `/data/filebrowser/files` — encrypted, host-level wiring.
- Timezone: `America/New_York`.
- linuxserver-style images use uid=1000 / gid=100 (ryan / users) for content ownership.
- Postgres images: pinned to specific major versions (immich = 14 with vectorchord, miniflux = 18-alpine, forgejo = whatever the legacy used).
- Newt joins these networks: `default`, `immich`, `forgejo`, `miniflux` (add yours if external tunnel access needed).
- gluetun is the WireGuard egress for the *arr stack via Mullvad.

---

## Output discipline

- **Always show the user the proposed file before writing it.** A draft module is a substantial commit; let the user sanity-check the image name, ports, sops keys, and dependencies first.
- **Never silently auto-add to `secrets.yaml`** — that file requires the user's sops key to edit. Tell them the exact entries to add.
- **Verify with `nix eval` before declaring done.** Use the placeholder dance for any new binary sops files, but **never overwrite real `nix/hosts/<host>/secrets/*` files**. Ask first if those paths already contain user-encrypted material.
- **Don't run `nixos-rebuild switch` or `nix run '.#deploy-rs'`** — those are system-state-changing actions the user should run themselves.
