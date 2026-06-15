source ../../work/mod.nu

const zoxide_init_path = "~/.zoxide.nu"
const carapace_init_path = "~/.cache/carapace/init.nu"

# add zoxide
if (has-command "zoxide") {
  ^zoxide init nushell | save -f $zoxide_init_path
  $env.ZOXIDE_INIT_PATH = $zoxide_init_path
} else {
    print 'zoxide not installed'
}

# set up carapace
if (has-command "carapace") {
  mkdir ~/.cache/carapace
  ^carapace _carapace nushell | save -f $carapace_init_path
  $env.CARAPACE_INIT_PATH = $carapace_init_path
} else {
  print "carapace not installed"
}

def has-command [cmd: string]: nothing -> bool {
  (which $cmd | length) > 0
}
