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

# The API's App schema has no server-side defaults: an add must carry every
# non-optional field, including resolved gstreamer pipelines and the render
# node — Wolf-version- and host-specific values nothing in nix can sensibly
# declare. Declared apps are therefore allowed to be partial; the gaps are
# filled from a live app record, whose values are valid for the running
# Wolf by construction. Declared fields win. The donor's id is never kept:
# ids only need to be unique within a profile (baseline cloning duplicates
# them across profiles), but inheriting the donor's would collide with the
# donor app itself when both land in the same profile.
def complete-app [app: record, donor: any] {
  let base = if $donor == null { {} } else { $donor }
  let merged = $base | merge $app
  let id = $app.id? | default ($merged.title | str lowercase | str replace --all " " "-")
  $merged | upsert id $id
}

def main [
  --file: string # desired-profiles spec rendered at build time (JSON list)
  --state-file: string # last-applied spec; lives next to Wolf's config.toml so wiping Wolf identity also resets it
  --template-cache-file: string # cached baseline profile record; consulted when the live baseline has been deleted from the picker
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
  mut template: any = if ($templates | is-empty) { null } else { $templates | first }
  if $template != null {
    if $template_cache_file != null {
      # Refresh while the live baseline exists: it's an ordinary profile
      # Wolf's UI lists in the picker, so it may be deliberately deleted —
      # the cache keeps rebuilds building from the full baseline anyway.
      $template | to json | save --force $template_cache_file
    }
  } else if ($template_cache_file != null) and ($template_cache_file | path exists) {
    $template = (try { open --raw $template_cache_file | from json } catch { null })
    if $template != null {
      log $"baseline profile '($BASELINE_PROFILE_ID)' not live, using cached template"
    }
  }
  if $template == null {
    log $"baseline profile '($BASELINE_PROFILE_ID)' not found, declared profiles get only their extra apps"
  }

  let donor_apps = if $template != null { $template | get apps } else {
    $current | each {|pr| $pr.apps? | default [] } | flatten
  }
  let donor = if ($donor_apps | is-empty) { null } else { $donor_apps | first }
  if $donor == null {
    log "no live app record to complete partial declared apps from; adds may be rejected"
  }

  mut applied = []
  for p in $desired {
    let existing = $current | where id == $p.id
    let prev = $last_applied | where id == $p.id

    # Only (re)build a profile when its nix spec differs from what was last
    # applied — runtime edits made via wolf-ui or the API survive otherwise.
    # Compared on the declared spec, not the completed payload, so donor
    # drift alone never triggers a rebuild.
    if (not ($existing | is-empty)) and (not ($prev | is-empty)) and (($prev | first) == $p) {
      $applied = ($applied | append $p)
      continue
    }

    let base_apps = if $template == null { [] } else {
      $template | get apps | where {|a| ($a.title? | default "") not-in $p.exclude_apps }
    }
    let extra_apps = $p.extra_apps | each {|a| complete-app $a $donor }
    mut profile = if $template == null {
      {id: $p.id, name: $p.name, icon_png_path: "", apps: ($base_apps ++ $extra_apps)}
    } else {
      $template | upsert id $p.id | upsert name $p.name | upsert apps ($base_apps ++ $extra_apps)
    }
    if "pin" in $profile { $profile = ($profile | reject pin) }
    if $p.pin != null { $profile = ($profile | upsert pin $p.pin) }
    if $p.icon_png_path != null { $profile = ($profile | upsert icon_png_path $p.icon_png_path) }

    let prior = if ($existing | is-empty) { null } else { $existing | first }
    if $prior != null {
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
      if $prior == null {
        log $"profile '($p.id)': add failed"
        continue
      }
      # A rejected add after a successful remove would otherwise leave the
      # profile deleted; put back the exact record that was removed — it
      # came from the API, so it round-trips.
      let restored = try { api-post $socket "/api/v1/profiles/add" ($prior | to json --raw) } catch { null }
      if $restored == null {
        log $"profile '($p.id)': add failed and restore failed, profile is gone from Wolf \(on-disk state untouched)"
      } else {
        log $"profile '($p.id)': add rejected, previous profile restored"
      }
      continue
    }
    $applied = ($applied | append $p)
  }

  $applied | to json | save --force $state_file
  log $"sync complete, ($applied | length) of ($desired | length) declared profiles in desired state"
}
