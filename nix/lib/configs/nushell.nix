# Pure nushell config text generator.
# Returns { config, env, generated-theme } where all values are strings.
#
# - config/env: wrapper files that source the static configs + generated theme
# - generated-theme: the color theme override
{
  colors,
  themeName,
  dotfilesConfigDir ? "~/.config/dotfiles/config/nushell",
}: {
  config = ''
    source ${dotfilesConfigDir}/config.nu
    source ~/.config/nushell/generated-theme.nu
  '';

  env = ''
    source ${dotfilesConfigDir}/env.nu
  '';

  generated-theme = import ../../modules/programs/nushell/theme.nix {
    inherit colors themeName;
  };
}
