# Pure zellij config text generator.
# Returns { config } where the value is a rendered config.kdl string.
#
# The theme components come from the same theme.nix the home-manager module
# uses; this file only adds the KDL serialization home-manager's toKDL
# normally performs, matching its output shape (alphabetized keys, tab
# indentation, hex values as quoted strings) so a generated config.kdl and a
# managed one stay diffable against each other.
{
  colors,
  colorOverrides ? {},
  defaultMode ? "normal",
  mouse ? true,
  # Rendered as `default_shell` when non-null. A bare command name is valid —
  # zellij resolves it via PATH.
  defaultShell ? null,
}: let
  inherit (builtins) attrNames concatStringsSep map;

  # Must not collide with a compiled-in zellij theme name; see the note in
  # modules/programs/zellij/default.nix.
  themeName = "base16";

  themeColors = import ../../modules/programs/zellij/theme.nix {
    inherit colors colorOverrides;
  };

  renderComponent = name: attrs:
    "\t\t${name} {\n"
    + concatStringsSep "\n" (map (k: "\t\t\t${k} \"${attrs.${k}}\"") (attrNames attrs))
    + "\n\t\t}";

  themeBlock =
    "themes {\n\t${themeName} {\n"
    + concatStringsSep "\n" (map (n: renderComponent n themeColors.${n}) (attrNames themeColors))
    + "\n\t}\n}";
in {
  config =
    concatStringsSep "\n" (
      ["default_mode \"${defaultMode}\""]
      ++ (
        if defaultShell != null
        then ["default_shell \"${defaultShell}\""]
        else []
      )
      ++ [
        # Mirrors the keybind set in modules/programs/zellij/default.nix; keep
        # both in sync.
        "keybinds {\n\tnormal {\n\t\tbind \"Alt y\" {\n\t\t\tCopyLastCommandOutput\n\t\t}\n\t}\n}"
        "mouse_mode ${
          if mouse
          then "true"
          else "false"
        }"
        "theme \"${themeName}\""
        themeBlock
      ]
    )
    + "\n";
}
