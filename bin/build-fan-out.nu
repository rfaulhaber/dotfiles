const default_hosts = [hyperion atlas vulcan janus pallas hecate prometheus]

def main [--sequential]: list<string> -> nothing {
  let build_hosts = $in | default $default_hosts

  if $sequential {
    for host in $build_hosts {
      print $"Building ($host)"
      nixos-rebuild --flake $".#($host)" build  
    }
  } else {
    for host in $build_hosts {
      let id = job spawn { nixos-rebuild --flake $".#($host)" build  }
      job describe $id $host
    }
  }
}
