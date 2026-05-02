#!/usr/bin/env nu
#
# Prepare atlas's docker-compose state for the new nix-managed OCI layout
# per nix/hosts/atlas/MIGRATION.org.
#
# What this script does for each service:
#   1. Stops the legacy container(s) that are using the source paths.
#   2. For every state directory: either creates a new dataset and moves
#      data in, renames an existing dataset, or repoints an existing
#      dataset's mountpoint — depending on the op kind (see below).
#   3. For multi-volume services (forgejo, immich): ensures the parent
#      dataset at data/apps/<svc> has mountpoint=none.
#   4. Collapses single-child parent layouts (miniflux, linkding) into
#      a single dataset via promote-to-parent's atomic 3-step rename.
#   5. For filebrowser: removes the empty residual db dataset, then
#      collapses the config child into the parent (which keeps the
#      encrypted files sibling).
#
# Idempotent: re-running after a partial run skips already-migrated
# services. Auto-recovers promote-to-parent runs that died between steps
# (detects ${parent}_collapse_tmp and resumes). Refuses to clobber
# non-empty destinations.
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
#   set-mountpoint      Set the mountpoint of an existing dataset and
#                       optionally apply other ZFS properties. Used when
#                       the dataset name is already correct and only its
#                       mountpoint or tunables need to change (immich,
#                       forgejo, syncthing's encrypted user-content).
#                       Fields: dataset, mountpoint, [properties].
#   promote-to-parent   Collapse a child dataset into its parent's name.
#                       Auto-selects between an atomic 3-step rename
#                       (only-child case) and mount-parent-first +
#                       cross-fs mv (parent has surviving siblings).
#                       Fields: child, parent, mountpoint, [properties], [chown].
#   destroy-if-empty    Surgical removal of an empty residual dataset
#                       (e.g., aborted-migration leftover). Allows empty
#                       snapshots (usedbysnapshots == 0) and uses
#                       `zfs destroy -r` to nuke them atomically.
#                       Fields: dataset.

def migration_plan [] {
  [
    # -- Wave 1: gluetun-routed *arrs (mv from /docker/config) ---------
    # gluetun has no migrate entry: only legacy state was a server-list
    # cache at the typo'd /docker/config/glueton (one 'u'). The new
    # dataset gets created fresh by zfs-manage on rebuild and gluetun
    # re-fetches the server list from Mullvad on first start. The
    # legacy /docker/config/glueton dir can be rm'd manually after.
    { service: "transmission"
      containers: ["transmission"]
      ops: [{ kind: "migrate", src: "/docker/config/transmission",
        dataset: "data/apps/transmission", dst: "/data/apps/transmission",
        chown: "1000:100" }] }
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
        # files dataset is encrypted (aes-256-gcm/raw). The keylocation
        # change moves the key from the legacy plain on-disk path to the
        # sops-rendered path; canmount=noauto opts the dataset out of
        # zfs-mount.service so the dedicated zfs-load-key-immich.service
        # is the sole code path that mounts it. encryption/keyformat are
        # create-only and not modified here — they ride with the dataset.
        { kind: "set-mountpoint", dataset: "data/apps/immich/files",
          mountpoint: "/data/apps/immich/files",
          properties: {
            canmount: "noauto"
            keylocation: "file:///run/secrets/immich/zfs-key"
          } }
        { kind: "set-mountpoint", dataset: "data/apps/immich/db",
          mountpoint: "/data/apps/immich/db",
          properties: { recordsize: "8K" } }
      ] }
    # Miniflux: collapse the only child (data/apps/miniflux/db) into its
    # empty parent (data/apps/miniflux). The 3-step rename via temp keeps
    # the operation atomic and avoids any data movement.
    { service: "miniflux"
      containers: ["miniflux", "miniflux_db"]
      ops: [
        { kind: "promote-to-parent",
          child: "data/apps/miniflux/db",
          parent: "data/apps/miniflux",
          mountpoint: "/data/apps/miniflux",
          properties: { recordsize: "8K" } }
      ] }

    # -- Wave 2 ----------------------------------------------------
    { service: "forgejo"
      containers: ["forgejo", "forgejo_db"]
      ops: [
        # Datasets are already at the right names (data/apps/forgejo/{data,db});
        # only their mountpoints need to flip from /data/forgejo/* to
        # /data/apps/forgejo/*. Same shape as immich.
        { kind: "ensure-parent", dataset: "data/apps/forgejo" }
        { kind: "set-mountpoint", dataset: "data/apps/forgejo/data",
          mountpoint: "/data/apps/forgejo/data" }
        { kind: "set-mountpoint", dataset: "data/apps/forgejo/db",
          mountpoint: "/data/apps/forgejo/db",
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
      ops: [
        # Empty residual dataset from a prior aborted migration attempt.
        # Must be removed before promote-to-parent so the parent's
        # "other surviving children" set is just [files].
        { kind: "destroy-if-empty", dataset: "data/apps/filebrowser/db" }
        # Collapse config (the actual SQLite + config) into the parent.
        # Parent has surviving sibling `files` (encrypted), so this uses
        # mv-staging rather than the 3-step rename.
        { kind: "promote-to-parent",
          child: "data/apps/filebrowser/config",
          parent: "data/apps/filebrowser",
          mountpoint: "/data/apps/filebrowser",
          properties: { recordsize: "64K" }
          chown: "1000:100" }
        # Encrypted user-content dataset: stays at /data/filebrowser/files
        # but flips off auto-mount and switches keylocation to the
        # sops-rendered path. encryption/keyformat are create-only and
        # ride with the dataset.
        { kind: "set-mountpoint", dataset: "data/apps/filebrowser/files",
          mountpoint: "/data/filebrowser/files",
          properties: {
            canmount: "noauto"
            keylocation: "file:///run/secrets/filebrowser/zfs-key"
          } }
      ] }
    # Linkding: same shape as miniflux — only-child collapse via 3-step rename.
    # No chown (linkding runs as root inside the container).
    { service: "linkding"
      containers: ["linkding"]
      ops: [
        { kind: "promote-to-parent",
          child: "data/apps/linkding/data",
          parent: "data/apps/linkding",
          mountpoint: "/data/apps/linkding",
          properties: { recordsize: "64K" } }
      ] }
    { service: "navidrome"
      containers: ["navidrome"]
      ops: [{ kind: "migrate", src: "/docker/config/navidrome",
        dataset: "data/apps/navidrome", dst: "/data/apps/navidrome",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    { service: "syncthing"
      containers: ["syncthing"]
      ops: [
        { kind: "migrate", src: "/docker/config/syncthing",
          dataset: "data/apps/syncthing", dst: "/data/apps/syncthing",
          chown: "1000:100" }
        # Encrypted synced-content datasets: stay at /data/sync and
        # /data/org but flip off auto-mount and switch keylocation to
        # the sops-rendered paths. Both are required by syncthing at
        # startup — the unlock units block podman-syncthing via the
        # `consumers` field on each encryptedDatasets entry.
        { kind: "set-mountpoint", dataset: "data/files/sync",
          mountpoint: "/data/sync",
          properties: {
            canmount: "noauto"
            keylocation: "file:///run/secrets/sync/zfs-key"
          } }
        { kind: "set-mountpoint", dataset: "data/files/org",
          mountpoint: "/data/org",
          properties: {
            canmount: "noauto"
            keylocation: "file:///run/secrets/org/zfs-key"
          } }
      ] }
    { service: "tautulli"
      containers: ["tautulli"]
      ops: [{ kind: "migrate", src: "/docker/config/tautulli",
        dataset: "data/apps/tautulli", dst: "/data/apps/tautulli",
        properties: { recordsize: "64K" }, chown: "1000:100" }] }
    # vikunja: no entry. The dataset is already at /data/apps/vikunja with
    # the right name and mountpoint; recordsize=64K gets applied by
    # zfs-manage-datasets.service on the rebuild. SQLite-only services
    # don't get the per-concern dataset split (that's for postgres tuning).
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
    move_all_entries $staging $dst
    let leftover = assert_dir_empty $staging "staging mv"
    if $leftover != null { return $leftover }
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

# Move every entry (regular files, dirs, dotfiles) from `src` into `dst`.
# Cross-filesystem-safe (delegates to /bin/mv which falls back to cp+rm
# when needed). Returns silently if `src` is empty (matches the
# `nullglob` semantics the previous bash version relied on).
# Errors propagate via get-or-fail — any partial move halts the script
# instead of silently destroying data downstream.
def move_all_entries [src: string, dst: string] {
  let entries = ls -a $src | get name
  if ($entries | is-empty) { return }
  ^mv -t $dst -- ...$entries | complete | get-or-fail $"mv ($entries | length) entries from ($src) → ($dst)"
}

# Defensive check after a move: refuse to proceed if anything was left
# behind. Pairs with destructive follow-up steps (zfs destroy of the
# source dataset) where remaining data would be lost forever.
def assert_dir_empty [dir: string, label: string]: nothing -> any {
  let remaining = ls -a $dir | get name
  if not ($remaining | is-empty) {
    let names = $remaining | each { |p| $p | path basename } | str join ", "
    print --stderr $"  ✗ ($dir) still has entries after ($label): ($names)"
    return { error: $"residual files in ($dir) after ($label)" }
  }
  null
}

# Promote a child dataset's content into its parent's name. Two strategies
# auto-selected based on whether the parent has any other surviving children:
#   - Only-child: atomic 3-step rename via a temporary sibling — no data
#     movement. zfs rename child temp → zfs destroy parent → zfs rename temp parent.
#   - Has other children: mount the parent at its target mountpoint, then
#     do a single cross-fs mv from the child's mountpoint into the parent
#     (one data copy, no staging dir to manage). After mv, the script
#     verifies the child mountpoint is empty before unmounting + destroying.
# Recovery: if the only-child run is interrupted between steps, the
# `${parent}_collapse_tmp` sibling dataset is detected on retry and the
# missing rename is resumed automatically.
# Idempotent: if the child no longer exists and the parent is at the right
# mountpoint, this is a no-op (just re-applies properties).
def op_promote_to_parent [op: record, dry_run: bool] {
  let child = $op.child
  let parent = $op.parent
  let target_mp = $op.mountpoint
  let props = $op | get --optional properties | default {}
  let chown = $op | get --optional chown

  let child_exists = (dataset exists $child)
  let parent_exists = (dataset exists $parent)
  let temp = $"($parent)_collapse_tmp"
  let temp_exists = (dataset exists $temp)

  # Recovery: a prior interrupted only-child run left ${parent}_collapse_tmp
  # behind. Resume the missing steps before continuing — without this, the
  # plain idempotency branch below could mis-apply the parent's mountpoint
  # while the data is still stranded in temp.
  if $temp_exists {
    print $"  ⚠ recovery: ($temp) exists from a prior interrupted run"
    if $parent_exists {
      print $"  + zfs destroy ($parent)  [recovery: complete step 2/3]"
      if not $dry_run {
        ^zfs destroy $parent | complete | get-or-fail $"zfs destroy ($parent) [recovery]"
      }
    }
    print $"  + zfs rename ($temp) ($parent)  [recovery: complete step 3/3]"
    if not $dry_run {
      ^zfs rename $temp $parent | complete | get-or-fail $"zfs rename ($temp) ($parent) [recovery]"
    }
    let cur_mp = if $dry_run { "" } else { dataset mountpoint $parent }
    if $cur_mp != $target_mp {
      print $"  + zfs set mountpoint=($target_mp) ($parent)  [was ($cur_mp)]"
      if not $dry_run {
        ^zfs set $"mountpoint=($target_mp)" $parent | complete
          | get-or-fail $"zfs set mountpoint=($target_mp) ($parent)"
      }
    }
    apply_props $parent $props $dry_run
    if $chown != null {
      chown_dst $target_mp $chown $dry_run
    }
    return "ok"
  }

  # Idempotency: child gone, parent already at target mountpoint
  if not $child_exists and $parent_exists {
    let mp = dataset mountpoint $parent
    if $mp == $target_mp {
      print $"  ✓ ($child) already promoted to ($parent) at ($target_mp)"
      apply_props $parent $props $dry_run
      if $chown != null {
        chown_dst $target_mp $chown $dry_run
      }
      return "ok"
    }
    print $"  + zfs set mountpoint=($target_mp) ($parent)  [was ($mp)]"
    if not $dry_run {
      ^zfs set $"mountpoint=($target_mp)" $parent | complete
        | get-or-fail $"zfs set mountpoint=($target_mp) ($parent)"
    }
    apply_props $parent $props $dry_run
    if $chown != null {
      chown_dst $target_mp $chown $dry_run
    }
    return "ok"
  }

  if not $parent_exists {
    print --stderr $"  ✗ parent dataset ($parent) does not exist"
    return { error: $"missing parent: ($parent)" }
  }
  if not $child_exists {
    print --stderr $"  ✗ child ($child) gone but parent ($parent) not at target mountpoint"
    return { error: $"unexpected partial state for ($parent)" }
  }
  if not ($child | str starts-with $"($parent)/") {
    print --stderr $"  ✗ ($child) is not a child of ($parent)"
    return { error: $"not a parent-child pair: ($child) vs ($parent)" }
  }

  # Other children of parent (excluding the one we're promoting)
  let other_children = ^zfs list -H -r -o name -d 1 $parent | complete
    | get stdout | lines
    | where { |l| $l != $parent and $l != $child }

  if ($other_children | is-empty) {
    # Only-child case: atomic 3-step rename. (`temp` and `temp_exists`
    # are computed at the top of the function; the recovery branch above
    # handles the case where temp already exists from a prior run.)
    print $"  + zfs rename ($child) ($temp)  [step 1/3: move child out of parent]"
    if not $dry_run {
      ^zfs rename $child $temp | complete | get-or-fail $"zfs rename ($child) ($temp)"
    }
    print $"  + zfs destroy ($parent)  [step 2/3: empty parent]"
    if not $dry_run {
      ^zfs destroy $parent | complete | get-or-fail $"zfs destroy ($parent)"
    }
    print $"  + zfs rename ($temp) ($parent)  [step 3/3: promote temp to parent's name]"
    if not $dry_run {
      ^zfs rename $temp $parent | complete | get-or-fail $"zfs rename ($temp) ($parent)"
    }
    let cur_mp = if $dry_run { "" } else { dataset mountpoint $parent }
    if $cur_mp != $target_mp {
      print $"  + zfs set mountpoint=($target_mp) ($parent)  [was ($cur_mp)]"
      if not $dry_run {
        ^zfs set $"mountpoint=($target_mp)" $parent | complete
          | get-or-fail $"zfs set mountpoint=($target_mp) ($parent)"
      }
    }
    apply_props $parent $props $dry_run
    if $chown != null {
      chown_dst $target_mp $chown $dry_run
    }
    return "ok"
  }

  # Has other children: mount the parent at its target first, then mv
  # the child's contents directly into the now-mounted parent. Cross-fs
  # mv (= copy + delete) so this scales with data size — fine for small
  # config datasets like filebrowser (1.3M); a snapshot/send approach
  # would be needed for multi-GB cases.
  let child_mp = dataset mountpoint $child
  if not ($child_mp | str starts-with "/") {
    print --stderr $"  ✗ child ($child) has non-path mountpoint ($child_mp); cannot move data"
    return { error: $"non-path child mountpoint: ($child_mp)" }
  }

  let cur_parent_mp = dataset mountpoint $parent
  if $cur_parent_mp != $target_mp {
    print $"  + zfs set mountpoint=($target_mp) ($parent)  [mount parent at target; was ($cur_parent_mp)]"
    if not $dry_run {
      ^zfs set $"mountpoint=($target_mp)" $parent | complete
        | get-or-fail $"zfs set mountpoint=($target_mp) ($parent)"
    }
  }

  print $"  + mv ($child_mp)/* ($target_mp)/  [cross-fs move: child → parent]"
  if not $dry_run {
    move_all_entries $child_mp $target_mp
    # Refuse to destroy the child dataset if any files were left behind —
    # destroy would take them with it. Aborts the op cleanly so the user
    # can investigate what's still in $child_mp before retrying.
    let leftover = assert_dir_empty $child_mp "cross-fs mv to parent"
    if $leftover != null { return $leftover }
  }

  let child_mounted = if $dry_run { "yes" } else {
    ^zfs get -H -o value mounted $child | complete | get stdout | str trim
  }
  if $child_mounted == "yes" {
    print $"  + zfs unmount ($child)  [now empty after mv]"
    if not $dry_run {
      ^zfs unmount $child | complete | get-or-fail $"zfs unmount ($child)"
    }
  }
  print $"  + zfs destroy ($child)  [empty; data is in parent]"
  if not $dry_run {
    ^zfs destroy $child | complete | get-or-fail $"zfs destroy ($child)"
  }

  apply_props $parent $props $dry_run
  if $chown != null {
    chown_dst $target_mp $chown $dry_run
  }
  "ok"
}

# Surgical removal of an empty residual dataset (e.g., aborted-migration
# leftover). Allows snapshots so long as they reference no unique data
# (usedbysnapshots == 0 means every snapshot is identical to the live
# dataset, which itself must be empty) — uses `zfs destroy -r` to nuke
# the dataset and its empty snapshots atomically. Refuses anything with
# real content, children, or non-empty snapshots.
# Idempotent: silently ok if the dataset is already gone.
def op_destroy_if_empty [op: record, dry_run: bool] {
  let ds = $op.dataset

  if not (dataset exists $ds) {
    print $"  ✓ ($ds) does not exist; nothing to destroy"
    return "ok"
  }

  let kids = ^zfs list -H -r -o name -d 1 $ds | complete
    | get stdout | lines | where { |l| $l != $ds }
  if not ($kids | is-empty) {
    print --stderr $"  ✗ ($ds) has child datasets; refusing to destroy"
    return { error: $"dataset has children: ($ds)" }
  }

  let usage = ^zfs get -Hp -o property,value usedbydataset,usedbysnapshots,usedbychildren $ds
    | complete | get stdout | lines
    | reduce -f {} { |l, acc|
        let parts = $l | split row "\t"
        $acc | upsert ($parts | get 0) (($parts | get 1) | into int)
      }

  # 1MB threshold for usedbydataset covers ZFS metadata overhead while
  # still being far below anything resembling actual data.
  if $usage.usedbydataset >= 1048576 {
    print --stderr $"  ✗ ($ds) has ($usage.usedbydataset) bytes of dataset content; refusing"
    return { error: $"dataset not empty: ($ds)" }
  }
  if $usage.usedbysnapshots != 0 {
    print --stderr $"  ✗ ($ds) snapshots reference ($usage.usedbysnapshots) unique bytes; refusing"
    return { error: $"snapshots have unique data: ($ds)" }
  }
  if $usage.usedbychildren != 0 {
    print --stderr $"  ✗ ($ds) has child usage; refusing"
    return { error: $"dataset has child usage: ($ds)" }
  }

  let snap_count = ^zfs list -H -t snapshot -o name -r $ds | complete
    | get stdout | lines | length
  let suffix = if $snap_count > 0 {
    $" + ($snap_count) empty snapshots"
  } else { "" }

  let mounted = if $dry_run { "yes" } else {
    ^zfs get -H -o value mounted $ds | complete | get stdout | str trim
  }
  if $mounted == "yes" {
    print $"  + zfs unmount ($ds)"
    if not $dry_run {
      ^zfs unmount $ds | complete | get-or-fail $"zfs unmount ($ds)"
    }
  }
  print $"  + zfs destroy -r ($ds)  [empty residual($suffix)]"
  if not $dry_run {
    ^zfs destroy -r $ds | complete | get-or-fail $"zfs destroy -r ($ds)"
  }
  "ok"
}

def run_op [op: record, mp_idx: record, dry_run: bool] {
  match $op.kind {
    "ensure-parent" => { op_ensure_parent $op $dry_run }
    "set-mountpoint" => { op_set_mountpoint $op $dry_run }
    "rename" => { op_rename $op $dry_run }
    "migrate" => { op_migrate $op $mp_idx $dry_run }
    "promote-to-parent" => { op_promote_to_parent $op $dry_run }
    "destroy-if-empty" => { op_destroy_if_empty $op $dry_run }
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
