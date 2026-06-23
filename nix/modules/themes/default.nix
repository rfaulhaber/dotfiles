{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.themes;
  schemePath = "${inputs.tt-schemes}/base16/${cfg.active}.yaml";
  resolveTheme = import ../../lib/configs/theme.nix {
    inherit pkgs inputs lib;
    themesDir = ./.;
  };

  c = cfg.colors.withHashtag;

  # Canonical theme attrset: base16 colors + semantic aliases + optional custom colors.
  # This is the single source of truth consumed by globals.nix, waybar, and any
  # future module that needs themed color variables.
  themeAttrs =
    {
      inherit (c) base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base0A base0B base0C base0D base0E base0F;
      background = c.base00;
      cursorBg = c.base05;
      cursorBorder = c.base05;
      cursorFg = c.base00;
      foreground = c.base05;
      selectionBg = c.base05;
      selectionFg = c.base00;
      textColor = c.base07;
      inherit (c) red green yellow blue cyan magenta;
    }
    // optionalAttrs (c ? base10) {
      inherit (c) base10 base11 base12 base13 base14 base15 base16 base17;
    }
    // optionalAttrs (c ? bg) {
      inherit (c) bg bg-alt fg fg-alt grey teal violet orange;
    }
    // optionalAttrs (c ? dark-cyan) {
      inherit (c) dark-cyan dark-blue;
    }
    // optionalAttrs (c ? bright-black) {
      inherit (c) bright-black bright-white;
    };

  scss = let
    inherit (builtins) concatStringsSep map;
    colorVars =
      themeAttrs
      |> attrsToList
      |> (map ({
        name,
        value,
      }: "\$${name}: ${value};"))
      |> (concatStringsSep "\n");
  in
    colorVars + "\n\$font-family: ${cfg.font};\n";
in {
  options.modules.themes = {
    active = mkOption {
      type = types.str;
      description = "The active theme.";
    };

    font = mkOption {
      type = types.str;
      description = "The system-wide font family.";
      default = "Hack Nerd Font Mono";
    };

    colors = mkOption {
      type = types.attrs;
      description = "Active color set.";
      default = {};
    };

    overrides = mkOption {
      type = types.attrsOf types.str;
      description = ''
        Host-level palette corrections merged over the resolved theme, keyed by
        color name with `#`-prefixed values (e.g. `{ yellow = "#e0af68"; }`).
        Use this for genuinely global fixes — a base16 named slot that doesn't
        match its label. For single-consumer tweaks, prefer that consumer's own
        `colorOverrides` so other consumers (and the terminal palette) are left
        untouched.
      '';
      default = {};
    };

    themeAttrs = mkOption {
      type = types.attrs;
      description = "Resolved theme color attrset with semantic aliases (all values have '#' prefix).";
      readOnly = true;
    };

    scss = mkOption {
      type = types.str;
      description = "Theme as SCSS variable declarations, ready for @use or direct inclusion.";
      readOnly = true;
    };
  };
  config = {
    assertions = [
      {
        assertion = builtins.pathExists schemePath;
        message = "${cfg.active} is not a valid theme! For valid themes, see: https://tinted-theming.github.io/tinted-gallery/";
      }
    ];

    modules.themes.colors = resolveTheme {
      themeName = cfg.active;
      inherit (cfg) overrides;
    };
    modules.themes.themeAttrs = themeAttrs;
    modules.themes.scss = scss;
  };
}
