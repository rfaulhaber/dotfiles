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
          (mkIf (desktopCfg.environment.type == "wayland") {
            pbcopy = "${pkgs.wl-clipboard}/bin/wl-copy";
            pbpaste = "${pkgs.wl-clipboard}/bin/wl-paste";
          })
          (mkIf (desktopCfg.environment.type == "x11") {
            pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
            pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
          })
        ]);
      };

      zoxide = mkIf cfg.zoxide.enable {
        enable = true;
      };

      # TODO there is an issue where if carapace is not enabled the
      # configuration cannot load correctly. carapace should be optional
      carapace = mkIf cfg.carapace.enable {
        enable = true;
      };
    };

    user.shell = mkIf cfg.setDefault pkgs.nushell;

    user.packages = with pkgs; [
      bat # bat is used as nushell's pager
      (mkIf (desktopCfg.environment.type == "x11") xclip)
      (mkIf (desktopCfg.environment.type == "wayland") wl-clipboard)
    ];
  };
}
