def main [source_dataset, target_dataset]: nothing -> nothing {
  # derive the latest snapshot name
  let latest_snapshot = ^zfs list -t snapshot $source_dataset -o name,creation --json
    | from json
    | get datasets
    | transpose name properties
    | sort-by -c { |r| $r.properties.properties.creation.value | date from-human  }
    | last
    | get properties
    | select snapshot_name dataset
    | $"($in.dataset)@($in.snapshot_name)"

  ^zfs send -R $latest_snapshot | ^zfs recv -F $target_dataset

  ignore
}
