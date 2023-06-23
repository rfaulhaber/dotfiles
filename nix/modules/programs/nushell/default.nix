{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.nushell;
  # nushellConfig = import ./config.nix {};
  # nushellEnv = import ./env.nix {};
in {
  options.modules.programs.nushell = {
    enable = mkEnableOption false;
    # TODO direnv should be its own module
    useDirenv = mkOption {
      description = "Enable if direnv should be used.";
      default = true;
      type = types.bool;
    };
    setDefault = mkOption {
      description = "Sets Nushell to be the default shell for the system user.";
      default = false;
      type = types.bool;
    };
  };

  config = mkIf cfg.enable {
    home.programs.nushell = {
      enable = true;
      # configFile = nushellConfig;
      # envFile = nushellEnv;
    };

    user.shell = mkIf cfg.setDefault pkgs.nushell;
  };
}
