source ~/.zoxide.nu

let direnv_pre_prompt = { ||
  let direnv = (direnv export json | from json)
  let direnv = if $direnv != null { $direnv } else { {} }
  $direnv | load-env
}

$env.config = ($env.config | update hooks.pre_prompt ($env.config.hooks.pre_prompt | append $direnv_pre_prompt))

if ("~/.cache/carapace/init.nu" | path exists) {
  source ~/.cache/carapace/init.nu
}
