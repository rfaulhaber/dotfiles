{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.retropie-nix;
in {
  options.modules.desktop.retropie-nix = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    modules.desktop.environment.gnome.enable = true;

    environment.systemPackages = with pkgs; [
      retroarchFull
    ];

    users.users.gamer = {
      isNormalUser = true;
      description = "The gamer user.";
      home = "/home/gamer";
      extraGroups = ["audio" "lp" "plugdev"];
    };

    services.displayManager.autoLogin = {
        enable = true;
        user = "gamer";
      };
  };
}
