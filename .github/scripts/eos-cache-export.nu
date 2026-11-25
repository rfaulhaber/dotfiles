# Exports the paths of a freshly built eos system that no public cache serves,
# as a signed binary cache directory for eos-cache-publish.nu to merge into
# vulcan's eos cache (nix/hosts/vulcan/eos-cache.nix).
#
# On a fresh runner every closure path was either substituted, carrying its
# cache's signature, or built here, and only the latter are marked ultimate.
# So `ultimate` selects exactly the paths eos would otherwise build itself.
#
# A binary cache refuses a path whose references it doesn't hold. Each
# reference outside the export (all served by cache.nixos.org or
# nix-community, which outrank this cache on eos) gets a placeholder narinfo
# for the length of the copy, removed before anything is uploaded.
def main [
  system: path    # the built darwin system
  out: path       # directory to write the binary cache into
  key_file: path  # nix signing key (eos-cache-1)
] {
  let closure = ^nix path-info --recursive $system | lines
  let info = ^nix path-info --json --json-format 2 ...$closure | from json
  let store = $info.storeDir
  let paths = $info.info | transpose name meta

  let exported = $paths | where meta.ultimate
  let exported_names = $exported | get name
  let external = $exported
    | each {|p| $p.meta.references }
    | flatten
    | uniq
    | where $it not-in $exported_names

  mkdir $out
  let placeholders = $paths | where name in $external | each {|p|
    let file = $out | path join $"($p.name | str substring 0..31).narinfo"
    [
      $"StorePath: ($store)/($p.name)"
      "URL: nar/placeholder"
      $"NarHash: ($p.meta.narHash)"
      $"NarSize: ($p.meta.narSize)"
      ""
    ] | str join (char nl) | save --force $file
    $file
  }

  let targets = $exported_names | each {|n| $"($store)/($n)" }
  ^nix copy --no-recursive --to $"file://($out)?secret-key=($key_file)&compression=zstd" ...$targets
  for f in $placeholders { rm $f }

  let nar_size = $exported | get meta.narSize | math sum | into filesize
  print $"exported ($exported | length) of ($paths | length) closure paths, ($nar_size) unpacked"
}
