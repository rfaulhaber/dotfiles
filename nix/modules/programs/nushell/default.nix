{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.programs.nushell;
  mkNuString = str: "'${str}'";
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
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    # TODO import config into nu configuration from here
    home.programs = {
      nushell = {
        enable = true;
        configFile.text = "source ${config.dotfiles.configDir}/nushell/config.nu";
        envFile.text = "source ${config.dotfiles.configDir}/nushell/env.nu";

        shellAliases = mkIf pkgs.stdenv.isLinux {
          pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
          pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
        };
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
