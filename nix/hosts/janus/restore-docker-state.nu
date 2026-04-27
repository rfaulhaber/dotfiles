#!/usr/bin/env nu
# Restore Docker volume contents into Podman volumes after the NixOS cutover.
# Inverse of capture-docker-state.nu. Reads $"<vol>.tar.gz" tarballs from
# --source-dir and rehydrates each matching Podman volume.
#
# Run on janus as root, AFTER `nix run .#deploy-rs .#janus` has activated and
# BEFORE starting podman-compose-janus-root.target with the migrated state.
# See CUTOVER.org Phase 3 (lines 297-322).
#
#   sudo systemctl stop podman-compose-janus-root.target
#   sudo nu restore-docker-state.nu
#   sudo systemctl start podman-compose-janus-root.target

def main [
  --source-dir: path = /root/janus-volume-migration
  --volumes: list<string> = [
    janus_netbird-mgmt
    janus_netbird-signal
    janus_netbird-letsencrypt
  ]
] {
  if (^id -u | into int) != 0 {
    print --stderr "Must be run as root (try: sudo nu restore-docker-state.nu)"
    exit 1
  }

  # Bail if the compose target is up — wiping live volumes will corrupt SQLite.
  let running = (^systemctl is-active podman-compose-janus-root.target | complete)
  if $running.stdout =~ "^active" {
    print --stderr "podman-compose-janus-root.target is active. Stop it first:"
    print --stderr "  sudo systemctl stop podman-compose-janus-root.target"
    exit 1
  }

  # Verify all tarballs exist before touching any volume.
  let missing = $volumes | where { |v| not ($source_dir | path join $"($v).tar.gz" | path exists) }
  if ($missing | is-not-empty) {
    print --stderr $"Missing tarballs in ($source_dir):"
    $missing | each { |v| print --stderr $"  ($v).tar.gz" }
    exit 1
  }

  for vol in $volumes {
    let archive = $"($vol).tar.gz"
    print $"==> Restoring ($vol) from ($source_dir)/($archive)"

    # Equivalent of `podman volume inspect $vol || podman volume create $vol`:
    # try-block swallows the inspect failure when the volume is missing.
    try { ^podman volume inspect $vol o> /dev/null } catch { ^podman volume create $vol o> /dev/null }

    # Wipe + extract via a throwaway alpine container. dotglob via .[!.]* so
    # hidden files (e.g. SQLite -wal/-shm siblings) get cleared too.
    (^podman run --rm
      -v $"($vol):/dst"
      -v $"($source_dir):/src:ro"
      alpine sh -c $"rm -rf /dst/* /dst/.[!.]* 2>/dev/null; tar xzf /src/($archive) -C /dst")
  }

  print ""
  print "==> Spot check (volume contents):"
  for vol in $volumes {
    print $"--- ($vol) ---"
    ^podman run --rm -v $"($vol):/x:ro" alpine ls -la /x
  }
}
