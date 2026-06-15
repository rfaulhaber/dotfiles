
if ($env.ZOXIDE_INIT_PATH? | default "" | path exists) {
  source "~/.zoxide.nu"
}

if ($env.CARAPACE_INIT_PATH? | default "" | path exists) {
  source ~/.cache/carapace/init.nu
}
