if (which zoxide | length) > 0 {
#  source ~/.zoxide.nu
}

let direnv_pre_prompt = { ||
  if (which direnv | is-empty) {
    return
  }

  direnv export json | from json | default {} | load-env
}

$env.config = ($env.config | update hooks.pre_prompt ($env.config.hooks.pre_prompt | append $direnv_pre_prompt))

if ("~/.cache/carapace/init.nu" | path exists) {
  source ~/.cache/carapace/init.nu
}
