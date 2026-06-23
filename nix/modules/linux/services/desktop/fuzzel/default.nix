{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.fuzzel;
  colors = config.modules.themes.colors;
  font = config.modules.themes.font;

  # fuzzel's ini wants bare (no-`#`) hex; normalise overrides so a host can pass
  # either form. Roles are merged so a single one (e.g. `match`) can be retuned
  # without restating the block — the base16 ramp leaves `match` and `selection`
  # near-identical on some schemes.
  palette =
    {
      background = colors.base00;
      text = colors.fg;
      match = colors.red;
      selection = colors.fg-alt;
      selection-text = colors.cyan;
      selection-match = colors.red;
      border = colors.teal;
    }
    // builtins.mapAttrs (_: lib.removePrefix "#") cfg.colorOverrides;
in {
  options.modules.desktop.fuzzel = {
    enable = mkEnableOption false;
    colorOverrides = mkOption {
      type = types.attrsOf types.str;
      description = ''
        Per-role overrides for fuzzel's color block, keyed by fuzzel role
        (`background`, `text`, `match`, `selection`, `selection-text`,
        `selection-match`, `border`). Values may include a leading `#`.
      '';
      default = {};
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      fuzzel
    ];

    home.configFile."fuzzel/fuzzel.ini".text = ''
      terminal = ghostty -e
      font = ${font}

      [colors]
      background=${palette.background}EE
      text=${palette.text}FF
      match=${palette.match}FF
      selection=${palette.selection}FF
      selection-text=${palette.selection-text}FF
      selection-match=${palette.selection-match}FF
      border=${palette.border}FF
    '';
  };
}
