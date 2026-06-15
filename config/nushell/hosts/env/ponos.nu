source ../../work/mod.nu

# add zoxide
if (has-command "zoxide") {
    ^zoxide init nushell | save -f ~/.zoxide.nu
} else {
    print 'zoxide not installed'
}

# set up carapace
if (has-command "carapace") {
  mkdir ~/.cache/carapace
  ^carapace _carapace nushell | save -f ~/.cache/carapace/init.nu
} else {
  print "carapace not installed"
}

def has-command [cmd: string]: nothing -> bool {
  (which $cmd | length) > 0
}
