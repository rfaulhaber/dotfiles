def main [spec?: string, --file: string, --dry-run] {
  # Prefer --file to avoid nushell's shebang-argv record-literal parsing, which
  # mangles JSON passed positionally (curly-braced argv gets parsed as a record
  # then re-serialized, yielding a double-encoded string).
  let config = if $file != null and $file != "" {
    open --raw $file
  } else {
    $spec | default $in
  }

  if $config == null {
    print --stderr "Null input. Exiting."
    exit 1
  }

  if (which ^zfs | length) == 0 {
    print --stderr "No ZFS executable. Exiting"
    exit 1
  }

  # Properties that ZFS makes immutable after the dataset exists. This list
  # gates the post-create `zfs set` loop — anything in here is skipped when
  # updating an existing dataset, since ZFS would reject the set. It does
  # NOT gate what gets passed at create time: new datasets receive every
  # property as a `-o` flag (see below), which is required for atomic
  # creation of encrypted datasets where `keyformat=raw` demands
  # `keylocation` be set in the same call.
  #
  # `keylocation` is intentionally NOT here: on an already-encrypted dataset
  # it stays mutable via `zfs set`, which is what lets us migrate a dataset's
  # key path from on-disk to a sops-rendered path under /run/secrets without
  # recreating the dataset.
  let immutable_after_create = [
    "casesensitivity" "normalization" "utf8only" "encryption" "keyformat"
  ]

  let config_json = $config
    | from json
    | transpose "dataset" "properties"

  let results = $config_json
    | each { |config|
        let dataset_name = $config | get dataset
        let properties = $config | get properties.properties
        let is_new = not ($dataset_name | dataset exists)

        if $is_new {
            # Pass ALL properties at create time. Encrypted datasets need
            # `encryption`, `keyformat`, and `keylocation` set atomically in
            # the same `zfs create` call — without `keylocation`, ZFS defaults
            # to `keylocation=prompt`, which for `keyformat=raw` tries to read
            # 32 bytes from stdin and fails immediately under systemd. Passing
            # mutable properties as `-o` flags is equivalent to a follow-up
            # `zfs set`, so there's no downside to bundling everything here.
            let create_opts = $properties
              | transpose "key" "value"
              | each { |p| [-o $"($p.key)=($p.value)"] }
              | flatten

            print $"Creating dataset ($dataset_name)"

            let result = if $dry_run {
                print $"[DEBUG]: would have run '^zfs create -p ($create_opts | str join ' ') ($dataset_name)'"
                { exit_code: 0 }
            } else {
                ^zfs create -p ...$create_opts $dataset_name | complete
            }

            if $result.exit_code != 0 {
                print --stderr $"Creating ($dataset_name) failed with status code ($result.exit_code)"
                let err = $result | get --optional stderr | default "no error message"
                print --stderr $err
                return { dataset: $dataset_name, result: $result }
            }
        } else {
            print $"Dataset ($dataset_name) exists"
        }

        # On existing datasets, apply the subset of properties ZFS still
        # accepts via `zfs set`. On newly-created datasets every property was
        # already applied via `-o` above, so this list is empty and the loop
        # is a no-op.
        let mutable_props = if $is_new {
            []
          } else {
            $properties
              | transpose "key" "value"
              | where { |p| $p.key not-in $immutable_after_create }
          }

        for pair in $mutable_props {
            let prop = $pair | get key
            let value = $pair | get value

            # Skip if the property already has the desired value
            let current = if $dry_run { null } else {
              ^zfs get -H -o value $prop $dataset_name | complete
                | if $in.exit_code == 0 { $in.stdout | str trim } else { null }
            }

            if $current == $value {
              print $"($prop)=($value) already set on ($dataset_name), skipping"
              continue
            }

            print $"setting ($prop)=($value) on ($dataset_name)"

            let result = if $dry_run {
              print $"[DEBUG]: would have run '^zfs set ($prop)=($value) ($dataset_name)'"
              { exit_code: 0 }
            } else {
              ^zfs set $"($prop)=($value)" $dataset_name | complete
            }

            if $result.exit_code != 0 {
                print --stderr $"Setting ($prop)=($value) on ($dataset_name) failed with status code ($result.exit_code)"
                let err = $result | get --optional stderr | default "no error message"
                print --stderr $err
                return { dataset: $dataset_name, result: $result }
            }
        }

        # Ensure the dataset is mounted — but only if it's actually
        # supposed to be auto-mounted. mountpoint=none/legacy datasets
        # have nowhere to mount; canmount=noauto/off datasets are
        # explicitly opted out of automatic mounting (e.g. encrypted
        # datasets unlocked by a dedicated systemd unit later).
        let mountpoint_now = if $dry_run { "/dry-run" } else {
          ^zfs get -H -o value mountpoint $dataset_name | complete
            | get stdout | str trim
        }
        let canmount_now = if $dry_run { "on" } else {
          ^zfs get -H -o value canmount $dataset_name | complete
            | get stdout | str trim
        }
        let should_auto_mount = (
          $mountpoint_now != "none"
          and $mountpoint_now != "legacy"
          and $canmount_now == "on"
        )

        if $should_auto_mount {
          let is_mounted = if $dry_run { "yes" } else {
            ^zfs get -H -o value mounted $dataset_name | complete
              | get stdout | str trim
          }

          if $is_mounted != "yes" {
            print $"Mounting ($dataset_name)"
            let result = if $dry_run {
              print $"[DEBUG]: would have run '^zfs mount ($dataset_name)'"
              { exit_code: 0 }
            } else {
              ^zfs mount $dataset_name | complete
            }

            if $result.exit_code != 0 {
              print --stderr $"Mounting ($dataset_name) failed with status code ($result.exit_code)"
              let err = $result | get --optional stderr | default "no error message"
              print --stderr $err
              return { dataset: $dataset_name, result: $result }
            }
          }

          # Ownership/mode live on the dataset's root inode, so this is a
          # one-time fix in practice — but re-checking each run keeps a
          # recreated dataset (fresh pool, reinstall) usable by its owner
          # without a manual chown. Only meaningful once mounted, hence
          # nested under the auto-mount branch.
          let owner = $config | get --optional properties.owner
          let group = $config | get --optional properties.group
          let mode = $config | get --optional properties.mode

          if $owner != null or $group != null {
            let want = $"($owner | default '')(if $group != null { $':($group)' } else { '' })"
            let have = if $dry_run { null } else {
              ^stat -c '%U:%G' $mountpoint_now | complete
                | if $in.exit_code == 0 { $in.stdout | str trim } else { null }
            }
            # stat always reports user:group; compare only the halves requested
            # so owner-only / group-only specs don't churn every run.
            let matches = (
              $have != null
              and ($owner == null or ($have | split row ':' | get 0) == $owner)
              and ($group == null or ($have | split row ':' | get 1) == $group)
            )

            if not $matches {
              print $"chown ($want) ($mountpoint_now) for ($dataset_name)"
              let result = if $dry_run {
                print $"[DEBUG]: would have run 'chown ($want) ($mountpoint_now)'"
                { exit_code: 0 }
              } else {
                ^chown $want $mountpoint_now | complete
              }

              if $result.exit_code != 0 {
                print --stderr $"chown ($want) on ($mountpoint_now) failed with status code ($result.exit_code)"
                let err = $result | get --optional stderr | default "no error message"
                print --stderr $err
                return { dataset: $dataset_name, result: $result }
              }
            }
          }

          if $mode != null {
            let have = if $dry_run { null } else {
              ^stat -c '%04a' $mountpoint_now | complete
                | if $in.exit_code == 0 { $in.stdout | str trim } else { null }
            }
            # Normalise "700" and "0700" to the same 4-digit form stat prints.
            let want = $mode | fill --alignment right --character '0' --width 4

            if $have != $want {
              print $"chmod ($want) ($mountpoint_now) for ($dataset_name)"
              let result = if $dry_run {
                print $"[DEBUG]: would have run 'chmod ($want) ($mountpoint_now)'"
                { exit_code: 0 }
              } else {
                ^chmod $want $mountpoint_now | complete
              }

              if $result.exit_code != 0 {
                print --stderr $"chmod ($want) on ($mountpoint_now) failed with status code ($result.exit_code)"
                let err = $result | get --optional stderr | default "no error message"
                print --stderr $err
                return { dataset: $dataset_name, result: $result }
              }
            }
          }
        }

        return { dataset: $dataset_name, result: "ok" }
    }

  mut non_zero_exit = false;

  for $result in $results {
    let exit_code = if ($result.result | describe) == "string" {
      0
    } else {
        $result | get --optional result.exit_code | default 0
    }

    if $exit_code != 0 {
        print $"Dataset ($result.dataset) failed: ($result.result.stderr)"
        $non_zero_exit = true
    } else {
        print $"Dataset ($result.dataset) succeeded"
    }
  }

  if $non_zero_exit {
    exit 1
  }
}

def "dataset exists" []: string -> bool {
  ^zfs list $in | complete | get exit_code | $in == 0
}
