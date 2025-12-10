def main [spec?: string, --dry-run] {
  # TODO delegate, process one dataset at a time to better capture errors
  let config = $spec | default $in

  if $config == null {
    print --stderr "Null input. Exiting."
    exit 1
  }

  if (which ^zfs | length) == 0 {
    print --stderr "No ZFS executable. Exiting"
    exit 1
  }

  let config_json = $config
    | from json
    | transpose "dataset" "properties"

  let results = $config_json
    | each { |config|
        let dataset_name = $config | get dataset
        let properties = $config | get properties.properties

        if not ($dataset_name | dataset exists) {
            print $"Creating dataset ($dataset_name)"

            let result = if $dry_run {
                print $"[DEBUG]: would have run '^zfs create ($dataset_name)'"
                { exit_code: 0 }
            } else {
                ^zfs create $dataset_name | complete
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

        for pair in ($properties | transpose "key" "value") {
            let prop = $pair | get key
            let value = $pair | get value

            print $"setting ($prop)=($value) on ($dataset_name)"

            let result = if $dry_run {
              print $"[DEBUG]: would have run '^zfs set ($prop)=($value) ($dataset_name)"
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
