{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.programs.nushell;
  desktopCfg = config.modules.desktop;
  isX11 = desktopCfg.environment.type == "x11";
  isWayland = desktopCfg.environment.type == "wayland";
in {
  options.modules.programs.nushell = {
    enable = mkEnableOption false;
    setDefault = mkOption {
      description = "Sets Nushell to be the default shell for the system user.";
      default = false;
      type = types.bool;
    };
    zoxide = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
    carapace = {
      enable = mkOption {
        type = types.bool;
        default = true;
      };
    };
    plugins = mkOption {
      type = types.listOf types.package;
      description = "Nushell plugins to include.";
      default = [  ];
    };
  };

  config = mkIf cfg.enable {
    # TODO import config into nu configuration from here
    home.programs = {
      nushell = let
        configDir = "${config.home.file.dotfiles.target}/config/nushell";
      in {
        enable = true;

        # given the way nushell loads, this causes all the sourced files in the
        # dotfiles to not load correctly. so the way we avoid that is to have
        # home manager's config load the config from the dotfiles like so
        configFile.text = "source ${configDir}/config.nu";
        envFile.text = "source ${configDir}/env.nu";

        shellAliases = mkIf (pkgs.stdenv.isLinux && desktopCfg.enable) (mkMerge [
          (mkIf isWayland {
            pbcopy = "${pkgs.wl-clipboard}/bin/wl-copy";
            pbpaste = "${pkgs.wl-clipboard}/bin/wl-paste";
          })
          (mkIf isX11 {
            pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
            pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
          })
        ]);

        plugins = cfg.plugins;
      };

      zoxide.enable = cfg.zoxide.enable;

      # TODO there is an issue where if carapace is not enabled the
      # configuration cannot load correctly. carapace should be optional
      carapace.enable = cfg.carapace.enable;
    };

    user.shell = mkIf cfg.setDefault pkgs.nushell;

    user.packages = with pkgs;
      [
        bat # bat is used as nushell's pager. see config/nushell/env.nu
      ]
      ++ lib.optional isX11 xclip
      ++ lib.optional isWayland wl-clipboard;
  };
}
