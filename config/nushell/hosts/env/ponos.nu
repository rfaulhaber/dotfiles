source ../../work/mod.nu

const zoxide_init_path = "~/.zoxide.nu"
const carapace_init_path = "~/.cache/carapace/init.nu"

# The generated nushell wrapper sources these files unconditionally at the top
# level (see nix/lib/configs/nushell.nix), so they must always exist — write an
# empty stub when the tool is missing to keep that parse-time source safe.
if (has-command "zoxide") {
  ^zoxide init nushell | save -f $zoxide_init_path
} else {
  print 'zoxide not installed'
  "" | save -f $zoxide_init_path
}

mkdir ~/.cache/carapace
if (has-command "carapace") {
  ^carapace _carapace nushell | save -f $carapace_init_path
} else {
  print "carapace not installed"
  "" | save -f $carapace_init_path
}

def has-command [cmd: string]: nothing -> bool {
  (which $cmd | length) > 0
}
