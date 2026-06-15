source ~/.config/dotfiles/config/nushell/config.nu
source ~/.config/nushell/generated-theme.nu

# zoxide and carapace init must be sourced at the top level: `source` is a
# parse-time keyword whose `def`/`alias` definitions are confined to the
# block they are parsed in, so nesting them inside the host `match`/`if` in
# config.nu drops the `z`/`zi` commands. env.nu generates these files (with
# empty stubs when a tool is absent). On home-manager hosts the equivalent
# top-level sourcing is injected by the nushell module instead.
source ~/.zoxide.nu
source ~/.cache/carapace/init.nu
