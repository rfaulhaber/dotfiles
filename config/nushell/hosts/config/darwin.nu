if (which zoxide | length) > 0 {
#  source ~/.zoxide.nu
}

let direnv_pre_prompt = { ||
  if (which direnv | is-empty) {
    return
  }

  direnv export json | from json | default {} | load-env
}

$env.config.hooks.env_change.PWD = (
  $env.config.hooks.env_change.PWD | append $direnv_pre_prompt
)

if ("~/.cache/carapace/init.nu" | path exists) {
  source ~/.cache/carapace/init.nu
}
