# Pure nushell config text generator.
# Returns { config, env, generated-theme } where all values are strings.
#
# - config/env: wrapper files that source the static configs + generated theme
# - generated-theme: the color theme override
{
  colors,
  themeName,
  colorOverrides ? {},
  dotfilesConfigDir ? "~/.config/dotfiles/config/nushell",
}: {
  config = ''
    source ${dotfilesConfigDir}/config.nu
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
  '';

  env = ''
    source ${dotfilesConfigDir}/env.nu
    setup-shell-integrations
  '';

  generated-theme = import ../../modules/programs/nushell/theme.nix {
    inherit colors themeName colorOverrides;
  };
}
