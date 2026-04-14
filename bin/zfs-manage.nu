def main [spec?: string, --dry-run] {
  let config = $spec | default $in

  if $config == null {
    print --stderr "Null input. Exiting."
    exit 1
  }

  if (which ^zfs | length) == 0 {
    print --stderr "No ZFS executable. Exiting"
    exit 1
  }

  # Properties that can only be set at creation time and cannot be changed afterwards.
  let create_only_props = [
    "casesensitivity" "normalization" "utf8only" "encryption" "keyformat" "keylocation"
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
            # Build creation options from create-only properties
            let create_opts = $properties
              | transpose "key" "value"
              | where { |p| $p.key in $create_only_props }
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

        # Apply mutable properties (skip create-only props on existing datasets)
        let mutable_props = $properties
          | transpose "key" "value"
          | where { |p| $is_new or ($p.key not-in $create_only_props) }

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

        # Ensure the dataset is mounted
        let is_mounted = if $dry_run { "yes" } else {
          ^zfs get -H -o value mounted $dataset_name | str trim
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
