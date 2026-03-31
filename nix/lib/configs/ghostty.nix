# Pure ghostty config text generator.
# Returns { config, theme } where both values are strings.
{
  colors,
  font,
  fontSize ? 16,
  extraConfig ? "",
}: let
  inherit (builtins) toString;
in {
  config =
    ''
      config-file = theme
      font-family = ${font}
      font-size = ${toString fontSize}
      window-inherit-working-directory = false
      shell-integration-features = cursor,sudo,title,ssh-env,ssh-terminfo
    ''
    + (
      if extraConfig != ""
      then extraConfig
      else ""
    );

  theme = ''
    background = ${colors.base00}
    foreground = ${colors.base05}
    cursor-color = ${colors.base05}
    selection-background = ${colors.base02}
    selection-foreground = ${colors.base05}

    palette = 0=${colors.base00}
    palette = 1=${colors.red}
    palette = 2=${colors.green}
    palette = 3=${colors.yellow}
    palette = 4=${colors.blue}
    palette = 5=${colors.magenta}
    palette = 6=${colors.cyan}
    palette = 7=${colors.base05}
    palette = 8=${colors.base03}
    palette = 9=${colors.bright-red}
    palette = 10=${colors.bright-green}
    palette = 11=${colors.base09}
    palette = 12=${colors.bright-blue}
    palette = 13=${colors.bright-magenta}
    palette = 14=${colors.bright-cyan}
    palette = 15=${colors.base07}
  '';
}
