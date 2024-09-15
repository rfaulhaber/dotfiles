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
        configFile.text = "source ${configDir}/config.nu";
        envFile.text = "source ${configDir}/env.nu";

        shellAliases = mkIf pkgs.stdenv.isLinux (mkMerge [
          (mkIf desktopCfg.wayland.enable {
            pbcopy = "${pkgs.wl-clipboard}/bin/wl-copy";
            pbpaste = "${pkgs.wl-clipboard}/bin/wl-paste";
          })
          (mkIf desktopCfg.xserver.enable {
            pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
            pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
          })
        ]);
      };

      # home.file.nushell_config = {
      #     source = "${config.dotfiles.configDir}/nushell";
      #     target = "${config.user.home}/.config/nushell";
      #     recursive = true;
      # };

      zoxide = mkIf cfg.zoxide.enable {
        enable = true;
      };

      carapace = mkIf cfg.carapace.enable {
        enable = true;
      };
    };

    user.shell = mkIf cfg.setDefault pkgs.nushell;

    environment.systemPackages = with pkgs; [
      bat # bat is used as nushell's pager
      xclip
    ];
  };
}
