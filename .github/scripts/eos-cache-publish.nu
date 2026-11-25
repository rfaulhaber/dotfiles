# Merges an eos cache artifact (eos-cache-export.nu) into the directory vulcan
# serves it from (nix/hosts/vulcan/eos-cache.nix).
#
# A reader fetches a narinfo and then the NAR it names, so NARs land first and
# every file appears by rename: no reader sees a narinfo whose NAR is missing,
# or a partly written file. Narinfos are always rewritten, which refreshes the
# mtime the prune timer expires them by; NARs are named by content hash, so an
# existing one is already right.

# The runner's UMask (0066) would leave new files unreadable to the web
# server, hence the explicit mode. The staging name is dot-prefixed so the
# prune timer can recognize one a cancelled job left behind.
def place [src: path, dest: path] {
  let staged = $dest | path dirname | path join $".($dest | path basename).tmp"
  ^cp $src $staged
  ^chmod 0644 $staged
  ^mv -f $staged $dest
}

def main [
  artifact: path  # downloaded eos-cache artifact
  dest: path      # served cache directory
] {
  let nars = glob ($artifact | path join "nar" "*")
  let new_nars = $nars | where {|n| not ($dest | path join "nar" ($n | path basename) | path exists) }
  for n in $new_nars { place $n ($dest | path join "nar" ($n | path basename)) }

  let narinfos = glob ($artifact | path join "*.narinfo")
  for i in $narinfos { place $i ($dest | path join ($i | path basename)) }
  place ($artifact | path join "nix-cache-info") ($dest | path join "nix-cache-info")

  print $"published ($narinfos | length) paths, ($new_nars | length) new NARs"
}
