{
  inputs,
  lib,
  ...
}: let
  inherit (lib) mkOption types mkEnableOption;
  nixos = import ./nixos.nix {inherit inputs lib;};
in
  nixos
  // {
    mkOpt = type: default: mkOption {inherit type default;};

    mkOptDesc = type: default: description:
      mkOption {inherit type default description;};

    writeNushellScriptBin = pkgs: name: text:
      pkgs.writeScriptBin name ''
        #!${pkgs.nushell}/bin/nu

        ${text}
      '';

    # Options shared between the linux (systemd) and darwin (launchd)
    # random-wallpaper modules. `interval` lives on each module because the
    # platforms accept different shapes (systemd timespan string vs. seconds).
    wallpaperCommonOptions = {
      enable = mkEnableOption "random wallpaper service";

      query = mkOption {
        type = types.str;
        description = "Optional query to pass to random wallpaper endpoint.";
        default = "";
      };

      perDisplay = mkOption {
        type = types.bool;
        default = false;
        description = ''
          When enabled, fetch a separate wallpaper for each connected display.
          Otherwise the same wallpaper is shown on every display.
        '';
      };

      token = mkOption {
        description = "API token for wallpaper API. A string is passed inline; a path is read at runtime.";
        type = types.either types.str types.path;
      };
    };

    # Decide between `--token` and `--token-file` based on whether the value
    # looks like a path. Used by both wallpaper modules.
    wallpaperTokenArgs = token:
      if builtins.isPath token || lib.hasPrefix "/" (toString token)
      then ["--token-file" (toString token)]
      else ["--token" token];
  }
