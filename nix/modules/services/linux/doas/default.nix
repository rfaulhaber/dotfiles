{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.doas;
in {
  options.modules.services.doas = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    security = {
      doas = {
        enable = true;
        extraRules = [
          {
            users = [config.user.name];
            persist = true;
            keepEnv = true;
          }
        ];
      };
      sudo.enable = false;
    };

    environment.systemPackages = with pkgs; [
      doas
      doas-sudo-shim
    ];
  };
}
