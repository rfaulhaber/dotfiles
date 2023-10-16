{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
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
    useZoxide = mkOption {
      description = "Enables Zoxide configuration with Nushell.";
      default = true;
      type = types.bool;
    };
  };

  config = mkIf cfg.enable {
    # TODO import config into nu configuration from here
    home.programs = {
      nushell = {
        enable = true;
        configFile.text = "source ${config.dotfiles.configDir}/nushell/config.nu";
        envFile.text = "source ${config.dotfiles.configDir}/nushell/env.nu";

        shellAliases = {
          pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
          pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
        };
      };

      zoxide = mkIf cfg.useZoxide {
        enable = true;
        enableNushellIntegration = true;
      };
    };

    user.shell = mkIf cfg.setDefault pkgs.nushell;

    environment.systemPackages = with pkgs; [
      bat # bat is used as nushell's pager
      xclip
    ];
  };
}
