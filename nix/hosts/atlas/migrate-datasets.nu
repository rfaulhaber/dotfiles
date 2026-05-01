#!/usr/bin/env nu
#
# Prepare atlas's docker-compose state for the new nix-managed OCI layout
# per nix/hosts/atlas/MIGRATION.org.
#
# What this script does for each service:
#   1. Stops the legacy container(s) that are using the source paths.
#   2. For every state directory: if the source is itself a ZFS dataset,
#      `zfs rename` it; otherwise create the target dataset, move the
#      data into its mountpoint, and chown if needed.
#   3. For multi-volume services (forgejo, immich, vikunja): ensures the
#      parent dataset at data/apps/<svc> exists with mountpoint=none.
#   4. Special-cases miniflux (rename data/apps/miniflux/db →
#      data/apps/miniflux, destroying the empty intermediate first) and
#      immich (data already lives at data/apps/immich/{files,db} —
#      just remount).
#
# Idempotent: re-running after a partial run skips already-migrated
# services. Refuses to clobber non-empty destinations.
#
# Run on atlas as root:
#   sudo nu nix/hosts/atlas/migrate-datasets.nu
#
# Useful flags:
#   --dry-run            preview every action without executing
#   --runtime podman     use podman instead of docker for `stop`
#   --no-stop            skip the container-stop step (you stopped them already)
#   <service> [<svc>...] migrate only the named services (default: all)

# ---------------------------------------------------------------- plan --
#
# Op kinds:
#   ensure-parent       Ensure a dataset with mountpoint=none exists.
#                       For multi-volume services that need a parent.
#                       Fields: dataset.
#   migrate             Move data from src into a named dataset whose
#                       mountpoint will be `dst`. Auto-detects whether
#                       the src is itself a dataset (uses zfs rename) or
#                       a regular directory (creates dataset, mvs files
#                       in via a temporary `<src>.migrate` shuffle).
#                       Fields: src, dataset, dst, [properties], [chown].
#   rename              Rename a dataset, optionally destroying an empty
#                       intermediate target dataset first.
#                       Fields: from, to, mountpoint, [properties].
#   set-mountpoint      Just set the mountpoint of an existing dataset
#                       (used for immich, where the children stay in
#                       place but their mountpoint changes).
#                       Fields: dataset, mountpoint, [properties].

def migration_plan [] {
  [
    # -- Wave 1: gluetun-routed *arrs (mv from /docker/config) ---------
    { service: "radarr"
      containers: ["radarr"]
      ops: [{ kind: "migrate", src: "/docker/config/radarr",
        dataset: "data/apps/radarr", dst: "/data/apps/radarr",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "sonarr"
      containers: ["sonarr"]
      ops: [{ kind: "migrate", src: "/docker/config/sonarr",
        dataset: "data/apps/sonarr", dst: "/data/apps/sonarr",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "prowlarr"
      containers: ["prowlarr"]
      ops: [{ kind: "migrate", src: "/docker/config/prowlarr",
        dataset: "data/apps/prowlarr", dst: "/data/apps/prowlarr",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "lidarr"
      containers: ["lidarr"]
      ops: [{ kind: "migrate", src: "/docker/config/lidarr",
        dataset: "data/apps/lidarr", dst: "/data/apps/lidarr",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "bazarr"
      containers: ["bazarr"]
      ops: [{ kind: "migrate", src: "/docker/config/bazarr",
        dataset: "data/apps/bazarr", dst: "/data/apps/bazarr",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "nzbget"
      containers: ["nzbget"]
      ops: [{ kind: "migrate", src: "/docker/config/nzbget",
        dataset: "data/apps/nzbget", dst: "/data/apps/nzbget",
        chown: "1000:100" }] }
    { service: "slskd"
      containers: ["slskd"]
      ops: [{ kind: "migrate", src: "/docker/config/slskd",
        dataset: "data/apps/slskd", dst: "/data/apps/slskd",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "soularr"
      containers: ["soularr"]
      ops: [{ kind: "migrate", src: "/docker/config/soularr",
        dataset: "data/apps/soularr", dst: "/data/apps/soularr",
        chown: "1000:100" }] }
    { service: "requestrr"
      containers: ["requestrr"]
      # requestrr runs as root inside the container — DO NOT chown
      ops: [{ kind: "migrate", src: "/docker/config/requestrr",
        dataset: "data/apps/requestrr", dst: "/data/apps/requestrr" }] }
    { service: "recyclarr"
      containers: ["recyclarr"]
      ops: [{ kind: "migrate", src: "/docker/config/recyclarr",
        dataset: "data/apps/recyclarr", dst: "/data/apps/recyclarr",
        chown: "1000:100" }] }

    # -- Wave 1: ZFS layout adjustments ------------------------------
    # Immich children already exist at data/apps/immich/{files,db};
    # only their mountpoint changes (and we add the parent w/ mountpoint=none).
    { service: "immich"
      containers: ["immich_server", "immich_postgres", "immich_redis"]
      ops: [
        { kind: "ensure-parent", dataset: "data/apps/immich" }
        { kind: "set-mountpoint", dataset: "data/apps/immich/files",
          mountpoint: "/data/apps/immich/files" }
        { kind: "set-mountpoint", dataset: "data/apps/immich/db",
          mountpoint: "/data/apps/immich/db",
          properties: { recordsize: "8K" } }
      ] }
    # Miniflux moves dataset name UP one level, since it's actually a
    # single-volume service. The intermediate data/apps/miniflux dataset
    # (auto-created by `zfs create -p`) is destroyed if empty.
    { service: "miniflux"
      containers: ["miniflux", "miniflux_db"]
      ops: [
        { kind: "rename", from: "data/apps/miniflux/db",
          to: "data/apps/miniflux", mountpoint: "/data/apps/miniflux",
          properties: { recordsize: "8K" } }
      ] }

    # -- Wave 2 ----------------------------------------------------
    { service: "forgejo"
      containers: ["forgejo", "forgejo_db"]
      ops: [
        { kind: "ensure-parent", dataset: "data/apps/forgejo" }
        # Forgejo's data + db dirs may currently be ZFS datasets at
        # /data/forgejo/{data,db}; `migrate` auto-detects and uses
        # `zfs rename` when applicable.
        { kind: "migrate", src: "/data/forgejo/data",
          dataset: "data/apps/forgejo/data",
          dst: "/data/apps/forgejo/data" }
        { kind: "migrate", src: "/data/forgejo/db",
          dataset: "data/apps/forgejo/db",
          dst: "/data/apps/forgejo/db",
          properties: { recordsize: "8K" } }
      ] }
    { service: "calibre"
      containers: ["calibre"]
      ops: [{ kind: "migrate", src: "/docker/config/calibre",
        dataset: "data/apps/calibre", dst: "/data/apps/calibre",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "calibre-web-auto"
      containers: ["calibre-web-auto"]
      ops: [{ kind: "migrate", src: "/docker/config/calibre-web",
        dataset: "data/apps/calibre-web", dst: "/data/apps/calibre-web",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "filebrowser"
      containers: ["filebrowser"]
      ops: [{ kind: "migrate", src: "/data/filebrowser/config",
        dataset: "data/apps/filebrowser", dst: "/data/apps/filebrowser",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "linkding"
      containers: ["linkding"]
      # linkding runs as root — DO NOT chown
      ops: [{ kind: "migrate", src: "/data/linkding/data",
        dataset: "data/apps/linkding", dst: "/data/apps/linkding",
        properties: { recordsize: "64K" } }] }
    { service: "navidrome"
      containers: ["navidrome"]
      ops: [{ kind: "migrate", src: "/docker/config/navidrome",
        dataset: "data/apps/navidrome", dst: "/data/apps/navidrome",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "syncthing"
      containers: ["syncthing"]
      ops: [{ kind: "migrate", src: "/docker/config/syncthing",
        dataset: "data/apps/syncthing", dst: "/data/apps/syncthing",
        chown: "1000:100" }] }
    { service: "tautulli"
      containers: ["tautulli"]
      ops: [{ kind: "migrate", src: "/docker/config/tautulli",
        dataset: "data/apps/tautulli", dst: "/data/apps/tautulli",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    # Vikunja's data is already at /data/apps/vikunja/{files,db} as
    # regular directories under the parent dataset — convert each child
    # dir into its own dataset.
    { service: "vikunja"
      containers: ["vikunja"]
      ops: [
        { kind: "ensure-parent", dataset: "data/apps/vikunja" }
        { kind: "migrate", src: "/data/apps/vikunja/files",
          dataset: "data/apps/vikunja/files",
          dst: "/data/apps/vikunja/files" }
        { kind: "migrate", src: "/data/apps/vikunja/db",
          dataset: "data/apps/vikunja/db",
          dst: "/data/apps/vikunja/db",
          properties: { recordsize: "64K" } }
      ] }
  ]
}

# ----------------------------------------------------------- helpers ---

# Build {host_path: dataset_name} for every mounted dataset on the host.
def zfs_mountpoint_index []: nothing -> record {
  let raw = ^zfs list -H -o name,mountpoint | complete
  if $raw.exit_code != 0 { return {} }
  $raw.stdout
    | lines
    | each { |l| $l | split row "\t" }
    | where { |r| ($r | length) == 2 and ($r | get 1) starts-with "/" }
    | reduce -f {} { |r, acc| $acc | upsert ($r | get 1) ($r | get 0) }
}

def "dataset exists" [name: string]: nothing -> bool {
  (^zfs list -H -o name $name | complete | get exit_code) == 0
}

def "dataset mountpoint" [name: string]: nothing -> string {
  ^zfs get -H -o value mountpoint $name | complete | get stdout | str trim
}

# A dataset is "empty intermediate" if it has no child datasets, no
# snapshots, and ~no used space (just the dataset metadata overhead).
def "dataset empty" [name: string]: nothing -> bool {
  let kids = ^zfs list -H -r -o name -d 1 $name | complete
    | get stdout | lines | where { |l| $l != $name }
  if not ($kids | is-empty) { return false }
  let snaps = ^zfs list -H -t snapshot -o name $name | complete
    | get stdout | lines
  if not ($snaps | is-empty) { return false }
  # 'used' returns a human-readable size; treat anything below 1M as empty.
  let used = ^zfs get -Hp -o value used $name | complete
    | get stdout | str trim | into int
  $used < 1048576
}

def "path empty-or-missing" []: string -> bool {
  let p = $in
  if not ($p | path exists) { return true }
  let kind = ($p | path type)
  if $kind != "dir" { return false }
  (^ls -A $p | complete | get stdout | str trim | is-empty)
}

def stop_containers [runtime: string, containers: list, dry_run: bool] {
  if ($containers | is-empty) { return }
  print $"  stopping containers: ($containers | str join ', ')"
  if $dry_run {
    print $"  [dry-run] ($runtime) stop ($containers | str join ' ')"
    return
  }
  if $runtime == "podman" {
    ^$runtime stop --ignore ...$containers | complete | ignore
  } else {
    for c in $containers {
      ^$runtime stop $c | complete | ignore
    }
  }
}

def "get-or-fail" [label: string]: record -> nothing {
  let r = $in
  if $r.exit_code != 0 {
    let err = $r | get --optional stderr | default "(no stderr)"
    print --stderr $"  ✗ ($label) failed: ($err | str trim)"
    error make { msg: $"($label) failed" }
  }
}

def apply_props [dataset: string, props: record, dry_run: bool] {
  for p in ($props | transpose key value) {
    let key = $p.key
    if $key == "mountpoint" { continue }  # handled separately
    let value = $p.value
    let current = if $dry_run { "" } else {
      ^zfs get -H -o value $key $dataset | complete | get stdout | str trim
    }
    if $current == $value { continue }
    print $"  + zfs set ($key)=($value) ($dataset)"
    if not $dry_run {
      ^zfs set $"($key)=($value)" $dataset | complete
        | get-or-fail $"zfs set ($key)=($value) ($dataset)"
    }
  }
}

# ------------------------------------------------------------- ops ---

def op_ensure_parent [op: record, dry_run: bool] {
  let ds = $op.dataset
  if (dataset exists $ds) {
    let mp = dataset mountpoint $ds
    if $mp == "none" {
      print $"  ✓ parent dataset ($ds) already exists with mountpoint=none"
      return "ok"
    }
    print $"  + zfs set mountpoint=none ($ds)  [was ($mp)]"
    if not $dry_run {
      ^zfs set mountpoint=none $ds | complete
        | get-or-fail $"zfs set mountpoint=none ($ds)"
    }
    return "ok"
  }
  print $"  + zfs create -o mountpoint=none ($ds)"
  if not $dry_run {
    ^zfs create -p -o mountpoint=none $ds | complete
      | get-or-fail $"zfs create ($ds)"
  }
  "ok"
}

def op_set_mountpoint [op: record, dry_run: bool] {
  let ds = $op.dataset
  let target = $op.mountpoint
  if not (dataset exists $ds) {
    print --stderr $"  ✗ dataset ($ds) does not exist"
    return { error: $"missing dataset: ($ds)" }
  }
  let current = dataset mountpoint $ds
  if $current != $target {
    print $"  + zfs set mountpoint=($target) ($ds)  [was ($current)]"
    if not $dry_run {
      ^zfs set $"mountpoint=($target)" $ds | complete
        | get-or-fail $"zfs set mountpoint=($target) ($ds)"
    }
  } else {
    print $"  ✓ ($ds) mountpoint already ($target)"
  }
  let props = $op | get --optional properties | default {}
  apply_props $ds $props $dry_run
  "ok"
}

def op_rename [op: record, dry_run: bool] {
  let from = $op.from
  let to = $op.to
  let target_mp = $op.mountpoint
  let props = $op | get --optional properties | default {}

  let from_exists = (dataset exists $from)
  let to_exists = (dataset exists $to)

  # Idempotency: rename already done
  if not $from_exists and $to_exists {
    let mp = dataset mountpoint $to
    if $mp == $target_mp {
      print $"  ✓ ($from) already renamed to ($to) at ($target_mp)"
      apply_props $to $props $dry_run
      return "ok"
    }
    print $"  ✓ ($to) exists; setting mountpoint=($target_mp)  [was ($mp)]"
    if not $dry_run {
      ^zfs set $"mountpoint=($target_mp)" $to | complete
        | get-or-fail $"zfs set mountpoint=($target_mp) ($to)"
    }
    apply_props $to $props $dry_run
    return "ok"
  }

  if not $from_exists {
    print --stderr $"  ✗ source dataset ($from) does not exist; ($to) is missing too"
    return { error: $"missing dataset: ($from)" }
  }

  # Both exist: destroy the empty intermediate target if applicable.
  if $to_exists {
    if not (dataset empty $to) {
      print --stderr $"  ✗ target ($to) exists with data; refusing to clobber"
      return { error: $"target dataset has data: ($to)" }
    }
    print $"  + zfs destroy ($to)  [empty intermediate]"
    if not $dry_run {
      ^zfs destroy $to | complete | get-or-fail $"zfs destroy ($to)"
    }
  }

  print $"  + zfs rename ($from) ($to)"
  if not $dry_run {
    ^zfs rename $from $to | complete | get-or-fail $"zfs rename ($from) ($to)"
  }

  let current = if $dry_run { "" } else { dataset mountpoint $to }
  if $current != $target_mp {
    print $"  + zfs set mountpoint=($target_mp) ($to)  [was ($current)]"
    if not $dry_run {
      ^zfs set $"mountpoint=($target_mp)" $to | complete
        | get-or-fail $"zfs set mountpoint=($target_mp) ($to)"
    }
  }

  apply_props $to $props $dry_run
  "ok"
}

def op_migrate [op: record, mp_idx: record, dry_run: bool] {
  let src = $op.src
  let dataset = $op.dataset
  let dst = $op.dst
  let props = $op | get --optional properties | default {}
  let chown = $op | get --optional chown

  let src_dataset = $mp_idx | get --optional $src
  let dst_exists = (dataset exists $dataset)
  let src_exists = ($src | path exists)

  # -- Idempotency: dataset exists with right mountpoint, src gone --
  if $dst_exists and not $src_exists {
    let cur_mp = dataset mountpoint $dataset
    if $cur_mp == $dst {
      print $"  ✓ ($dataset) already at ($dst); src ($src) gone"
      apply_props $dataset $props $dry_run
      if $chown != null {
        chown_dst $dst $chown $dry_run
      }
      return "ok"
    }
  }

  if not $src_exists {
    if $dst_exists {
      print --stderr $"  ✗ src ($src) missing and ($dataset) exists but mountpoint != ($dst)"
      return { error: $"unexpected partial state for ($dataset)" }
    }
    print --stderr $"  ✗ src ($src) missing — nothing to migrate"
    return { error: $"source missing: ($src)" }
  }

  # -- Case 1: src is itself a ZFS dataset → zfs rename -----------
  if $src_dataset != null {
    if $dst_exists {
      print --stderr $"  ✗ both ($src_dataset) and ($dataset) exist as datasets — refusing"
      return { error: $"both src and dst datasets exist" }
    }
    print $"  + zfs rename ($src_dataset) ($dataset)  [src is a dataset]"
    if not $dry_run {
      ^zfs rename $src_dataset $dataset | complete
        | get-or-fail $"zfs rename ($src_dataset) ($dataset)"
    }
    let cur_mp = if $dry_run { "" } else { dataset mountpoint $dataset }
    if $cur_mp != $dst {
      print $"  + zfs set mountpoint=($dst) ($dataset)  [was ($cur_mp)]"
      if not $dry_run {
        ^zfs set $"mountpoint=($dst)" $dataset | complete
          | get-or-fail $"zfs set mountpoint=($dst) ($dataset)"
      }
    }
    apply_props $dataset $props $dry_run
    if $chown != null {
      chown_dst $dst $chown $dry_run
    }
    return "ok"
  }

  # -- Case 2: src is a regular dir → create dataset, mv files in --
  let staging = $"($src).migrate"
  if ($staging | path exists) {
    print --stderr $"  ✗ staging path ($staging) already exists from a prior run — abort"
    return { error: $"staging exists: ($staging)" }
  }

  print $"  + mv ($src) ($staging)  [staging]"
  if not $dry_run {
    ^mv $src $staging | complete | get-or-fail $"mv ($src) ($staging)"
  }

  if not $dst_exists {
    print $"  + zfs create -o mountpoint=($dst) ($dataset)"
    if not $dry_run {
      ^zfs create -p -o $"mountpoint=($dst)" $dataset | complete
        | get-or-fail $"zfs create ($dataset)"
    }
  }

  print $"  + mv ($staging)/* ($dst)/"
  if not $dry_run {
    # Use shell glob via ^bash -c to handle dotfiles cleanly.
    ^bash -c $"shopt -s dotglob nullglob && mv ($staging)/* ($dst)/ 2>/dev/null; true" | complete | ignore
    ^rmdir $staging | complete | get-or-fail $"rmdir ($staging)"
  }

  apply_props $dataset $props $dry_run
  if $chown != null {
    chown_dst $dst $chown $dry_run
  }
  "ok"
}

def chown_dst [dst: string, chown: string, dry_run: bool] {
  print $"  + chown -R ($chown) ($dst)"
  if not $dry_run {
    ^chown -R $chown $dst | complete | get-or-fail $"chown -R ($chown) ($dst)"
  }
}

def run_op [op: record, mp_idx: record, dry_run: bool] {
  match $op.kind {
    "ensure-parent" => { op_ensure_parent $op $dry_run }
    "set-mountpoint" => { op_set_mountpoint $op $dry_run }
    "rename" => { op_rename $op $dry_run }
    "migrate" => { op_migrate $op $mp_idx $dry_run }
    _ => {
      print --stderr $"  ✗ unknown op kind: ($op.kind)"
      { error: $"unknown op kind: ($op.kind)" }
    }
  }
}

# ------------------------------------------------------------- main ---

def main [
  ...services: string             # services to migrate; empty = all
  --dry-run                       # preview without executing
  --runtime: string = "docker"    # container runtime for `stop`
  --no-stop                       # skip the container-stop step
] {
  if (^id -u | str trim) != "0" and not $dry_run {
    print --stderr "Must run as root (or use --dry-run for a preview)."
    exit 1
  }

  let plan = migration_plan
  let selected = if ($services | is-empty) {
    $plan
  } else {
    $plan | where { |e| $e.service in $services }
  }

  if ($selected | is-empty) {
    print --stderr $"No matching services. Known: ($plan | get service | str join ', ')"
    exit 1
  }

  let mp_idx = zfs_mountpoint_index
  print $"Migration plan: ($selected | get service | str join ', ')"
  if $dry_run { print "  [dry-run mode — no changes will be made]" }

  mut summary = []

  for entry in $selected {
    print ""
    print $"── ($entry.service) ──"

    if not $no_stop {
      stop_containers $runtime $entry.containers $dry_run
    }

    mut had_error = null
    for op in $entry.ops {
      let result = try {
        run_op $op $mp_idx $dry_run
      } catch { |err|
        { error: $err.msg }
      }
      let cols = try { $result | columns } catch { [] }
      if "error" in $cols {
        $had_error = $result.error
        break
      }
    }

    $summary = ($summary | append {
      service: $entry.service
      status: (if $had_error == null { "ok" } else { "failed" })
      detail: ($had_error | default "")
    })
  }

  print ""
  print "── summary ──"
  print ($summary | table --expand)

  let failures = $summary | where status == "failed"
  if not ($failures | is-empty) {
    exit 1
  }

  print ""
  print "All requested services migrated. Next:"
  print "  sudo nixos-rebuild switch --flake .#atlas"
}
