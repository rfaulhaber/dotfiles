# Ensure declared env entries on every docker app in Wolf's config.toml.
#
# Wolf has no global session env, and its management API hides the
# moonlight profile whose default app is Wolf UI, so the file is the only
# place every app can be reached. Runs before the Wolf container starts:
# Wolf loads the patched file and carries the entries through its own
# later rewrites. The file is only rewritten when an entry is missing or
# stale, after a backup beside it.
#
# wolf-profiles-sync's cached baseline profile is patched too. The sync
# rebuilds profiles from that cache once the live baseline is deleted, and
# a cache written before an env entry was declared would otherwise strip
# it from every app of the rebuilt profile.
#
# Best-effort like wolf-profiles-sync: every failure logs to stderr and
# exits 0, since a patch hiccup must not keep Wolf from starting.

def log [msg: string] {
  print --stderr $"wolf-session-env: ($msg)"
}

def env-key [entry: string] {
  $entry | split row "=" | first
}

# Declared keys are rewritten in place and missing ones appended, so an
# already-patched list comes back identical and triggers no rewrite.
def patch-env [entries: list, wanted: table] {
  let updated = $entries | each {|e|
    let match = $wanted | where key == (env-key $e)
    if ($match | is-empty) { $e } else { $"($match.0.key)=($match.0.value)" }
  }
  let present = $entries | each {|e| env-key $e }
  let missing = $wanted | where key not-in $present | each {|w| $"($w.key)=($w.value)" }
  $updated ++ $missing
}

def patch-app [app: record, wanted: table] {
  let runner = $app.runner? | default {}
  if ($runner.type? | default "") != "docker" { return $app }
  $app | upsert runner ($runner | upsert env (patch-env ($runner.env? | default []) $wanted))
}

def patch-profile [profile: record, wanted: table] {
  if "apps" not-in ($profile | columns) { return $profile }
  $profile | upsert apps ($profile.apps | each {|a| patch-app $a $wanted })
}

def patch-config [config: string, wanted: table] {
  if not ($config | path exists) {
    log $"($config) does not exist yet; Wolf creates it on first start and it is patched on the next"
    return
  }
  let current = open --raw $config | from toml
  if "profiles" not-in ($current | columns) {
    log $"($config) has no profiles, nothing to patch"
    return
  }
  let patched = $current | upsert profiles ($current.profiles | each {|p| patch-profile $p $wanted })
  if $patched == $current { return }

  # The file also holds Wolf's pairing state: refuse anything that doesn't
  # read back as exactly the intended record.
  let text = $patched | to toml
  if ($text | from toml) != $patched {
    error make {msg: "re-serialized config does not round-trip, leaving it untouched"}
  }
  ^cp --preserve=mode,ownership $config $"($config).session-env.bak"
  $text | save --force $config
  log $"set ($wanted.key | str join ', ') on every docker app in ($config)"
}

def patch-template-cache [cache: string, wanted: table] {
  if not ($cache | path exists) { return }
  let current = open --raw $cache | from json
  let patched = patch-profile $current $wanted
  if $patched == $current { return }
  $patched | to json | save --force $cache
  log $"set ($wanted.key | str join ', ') on every docker app in ($cache)"
}

def main [
  --config: string # Wolf's config.toml, by host-side path
  --template-cache-file: string # wolf-profiles-sync's cached baseline profile
  --spec: string # declared env, rendered at build time as a JSON object
] {
  let wanted = try { open --raw $spec | from json | transpose key value } catch { null }
  if $wanted == null {
    log $"cannot read ($spec), skipping"
    return
  }
  try { patch-config $config $wanted } catch {|err| log $"($config) skipped: ($err.msg)" }
  if $template_cache_file != null {
    try { patch-template-cache $template_cache_file $wanted } catch {|err| log $"($template_cache_file) skipped: ($err.msg)" }
  }
}
