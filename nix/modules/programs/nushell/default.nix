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
    home.programs = {
      nushell = {
        enable = true;
        configFile.source = "${config.dotfiles.dir}/config/nushell/config.nu";
        envFile.source = "${config.dotfiles.dir}/config/nushell/env.nu";

        shellAliases = {
          l = "ls -la";
          pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
          pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
        };

        extraEnv = let
          extraPaths = concatStringsSep ", " (builtins.map mkNuString [
            "${config.user.home}/.emacs.d/bin"
          ]);
        in ''
          let-env PATH = ($env.PATH | split row (char esep) | append [${extraPaths}])
        '';
      };

      zoxide = mkIf cfg.useZoxide {
        enable = true;
        enableNushellIntegration = true;
      };
    };

    user.shell = mkIf cfg.setDefault pkgs.nushell;

    environment.systemPackages = with pkgs; [xclip];
  };
}
