{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.awesome;
in {
  options.modules.desktop.awesome = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    services.xserver.windowManager.awesome = {enable = true;};

    modules.desktop.lightdm = {
      enable = true;
      defaultSession = "none+awesome";
    };

    home.configFile."awesome" = {
      source = "${config.dotfiles.configDir}/awesome";
      target = "./awesome";
      recursive = true;
    };

    environment.systemPackages = with pkgs; [
      lua-language-server
      stylua
    ];
  };
}
