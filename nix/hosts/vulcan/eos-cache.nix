# Binary cache for eos, the fleet's only aarch64-darwin host, which no machine
# here can build. build-and-cache.yml builds eos on a GitHub-hosted macOS
# runner, exports the closure paths no public cache serves as a signed binary
# cache, and a follow-up job on one of vulcan's runners merges that into
# `dir` (.github/scripts/eos-cache-{export,publish}.nu). eos reaches it over
# netbird as http://vulcan.netbird.selfhosted:4966.
#
# A plain directory rather than vulcan's /nix/store on purpose. A store keeps
# no path without its whole closure, so importing would also pull the ~8 GB
# darwin closure from cache.nixos.org, and harmonia would re-sign every
# imported path with the key the whole fleet trusts. Here the eos-cache key is
# trusted by eos alone, and vulcan's nix daemon never sees these paths.
{
  lib,
  pkgs,
  ...
}: let
  dir = "/var/lib/eos-cache";
  group = "eos-cache";

  # Every publish rewrites the narinfos of the build it carries, so a
  # narinfo's mtime is the last time any eos build contained that path.
  # Expire those, then the NARs no remaining narinfo names.
  prune = lib.my.writeNushellScriptBin pkgs "eos-cache-prune" ''
    def main [dir: path, retention_days: int] {
      let now = date now
      let entries = ls --all $dir
      let narinfos = $entries | where ($it.name | str ends-with ".narinfo")
      let cutoff = $now - ($retention_days * 1day)
      for f in ($narinfos | where modified < $cutoff) { rm --permanent $f.name }

      let live = $narinfos
        | where modified >= $cutoff
        | each {|f|
            open --raw $f.name
              | lines
              | where ($it | str starts-with "URL: ")
              | each {|l| $dir | path join ($l | str substring 5..) }
          }
        | flatten

      # Publishing writes a NAR before the narinfo naming it, and stages
      # every file under a dot-prefixed name, so anything under a day old
      # may belong to a publish still in flight.
      let settled = $now - 1day
      let orphans = ls --all ($dir | path join "nar")
        | where modified < $settled and name not-in $live
      let stale_tmp = $entries
        | where type == file and modified < $settled and ($it.name | path basename | str starts-with ".")
      for f in ($orphans | append $stale_tmp) { rm --permanent $f.name }

      print $"expired ($narinfos | where modified < $cutoff | length) narinfos, removed ($orphans | length) NARs"
    }
  '';
in {
  users.groups.${group} = {};

  # Each runner instance is its own DynamicUser. A group-writable directory
  # lets any of them rename over files another instance created, and setgid
  # keeps those files in the group.
  systemd.tmpfiles.rules = [
    "d ${dir} 2775 root ${group} -"
    "d ${dir}/nar 2775 root ${group} -"
  ];

  modules.services.github-runner.serviceOverrides = {
    SupplementaryGroups = [group];
    ReadWritePaths = [dir];
  };

  # No lan0 firewall opening: eos reaches this over netbird only.
  services.static-web-server = {
    enable = true;
    listen = "[::]:4966";
    root = dir;
    # NARs are already zstd, and narinfos are a few hundred bytes.
    configuration.general.compression = false;
  };

  systemd.services.eos-cache-prune = {
    description = "Expire paths no recent eos build has published";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${prune}/bin/eos-cache-prune ${dir} 14";
      ProtectSystem = "strict";
      ReadWritePaths = [dir];
      PrivateNetwork = true;
    };
  };

  systemd.timers.eos-cache-prune = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };
}
