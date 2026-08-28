source /nix/store/91l2dn0djqjfxkkgm86zip6lwazrypms-nushell-config/config.nu
source ($nu.default-config-dir | path join "generated-theme.nu")

# zoxide and carapace init must be sourced at the top level: `source` is a
# parse-time keyword whose `def`/`alias` definitions are confined to the
# block they are parsed in, so nesting them inside the host `match`/`if` in
# config.nu drops the `z`/`zi` commands. setup-shell-integrations (called
# from env below) generates these files, with empty stubs when a tool is
# absent. On home-manager hosts this wrapper is unused — its nushell module
# injects the equivalent top-level sourcing instead.
source ~/.zoxide.nu
source ~/.cache/carapace/init.nu
