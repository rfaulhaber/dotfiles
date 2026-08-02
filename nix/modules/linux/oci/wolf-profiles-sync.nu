# Reconcile declared Wolf profiles against Wolf's management API socket.
#
# Wolf owns config.toml (it rewrites the whole file on every pairing), so
# desired profiles are applied through the API instead of templating the
# file. Each declared profile is built from the image-default baseline
# profile's app list; cloning the live record rather than vendoring app
# definitions keeps unknown fields round-tripping untouched.
#
# Best-effort on purpose: every failure path logs to stderr and exits 0.
# This unit runs during system activation, and a profile-sync hiccup must
# never fail — and thereby roll back — a whole deploy.

const BASELINE_PROFILE_ID = "user"

def log [msg: string] {
  print --stderr $"wolf-profiles-sync: ($msg)"
}

def api-get [socket: string, path: string] {
  ^curl --silent --fail --unix-socket $socket $"http://localhost($path)"
}

def api-post [socket: string, path: string, body: string] {
  ^curl --silent --fail --unix-socket $socket --header "Content-Type: application/json" --data $body $"http://localhost($path)"
}

def main [
  --file: string # desired-profiles spec rendered at build time (JSON list)
  --state-file: string # last-applied spec; lives next to Wolf's config.toml so wiping Wolf identity also resets it
  --socket: string = "/tmp/sockets/wolf.sock"
] {
  let desired = open --raw $file | from json

  # The socket appears shortly after the container starts, but the API may
  # not answer immediately; poll instead of racing it.
  # Typed `any` because initializing with null would otherwise fix the
  # static type to nothing and fail parsing at the pipelines below.
  mut profiles_res: any = null
  mut attempts = 0
  while ($profiles_res == null) and ($attempts < 40) {
    $profiles_res = try { api-get $socket "/api/v1/profiles" | from json } catch { null }
    if $profiles_res == null { sleep 3sec }
    $attempts += 1
  }
  if $profiles_res == null {
    log $"management API at ($socket) never became ready, skipping sync"
    exit 0
  }
  # The server excludes the special moonlight profile from this listing, so
  # nothing below can ever touch it.
  let current = $profiles_res | get profiles

  let last_applied = if ($state_file | path exists) {
    try { open --raw $state_file | from json } catch { [] }
  } else { [] }

  let templates = $current | where id == $BASELINE_PROFILE_ID
  let template = if ($templates | is-empty) { null } else { $templates | first }
  if $template == null {
    log $"baseline profile '($BASELINE_PROFILE_ID)' not found, declared profiles get only their extra apps"
  }

  mut applied = []
  for p in $desired {
    let existing = $current | where id == $p.id
    let prev = $last_applied | where id == $p.id

    # Only (re)build a profile when its nix spec differs from what was last
    # applied — runtime edits made via wolf-ui or the API survive otherwise.
    if (not ($existing | is-empty)) and (not ($prev | is-empty)) and (($prev | first) == $p) {
      $applied = ($applied | append $p)
      continue
    }

    let base_apps = if $template == null { [] } else {
      $template | get apps | where {|a| ($a.title? | default "") not-in $p.exclude_apps }
    }
    mut profile = if $template == null {
      {id: $p.id, name: $p.name, icon_png_path: "", apps: ($base_apps ++ $p.extra_apps)}
    } else {
      $template | upsert id $p.id | upsert name $p.name | upsert apps ($base_apps ++ $p.extra_apps)
    }
    if "pin" in $profile { $profile = ($profile | reject pin) }
    if $p.pin != null { $profile = ($profile | upsert pin $p.pin) }
    if $p.icon_png_path != null { $profile = ($profile | upsert icon_png_path $p.icon_png_path) }

    if not ($existing | is-empty) {
      # The API has no update endpoint; replace is remove + add.
      log $"profile '($p.id)': spec changed, rebuilding"
      let removed = try { api-post $socket "/api/v1/profiles/remove" ({id: $p.id} | to json --raw) } catch { null }
      if $removed == null {
        log $"profile '($p.id)': remove failed, leaving as-is"
        continue
      }
    } else {
      log $"profile '($p.id)': creating"
    }

    let added = try { api-post $socket "/api/v1/profiles/add" ($profile | to json --raw) } catch { null }
    if $added == null {
      log $"profile '($p.id)': add failed"
      continue
    }
    $applied = ($applied | append $p)
  }

  $applied | to json | save --force $state_file
  log $"sync complete, ($applied | length) of ($desired | length) declared profiles in desired state"
}
