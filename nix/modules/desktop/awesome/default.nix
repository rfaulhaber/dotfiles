{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.awesome;
in {
  options.modules.desktop.awesome = {
    enable = mkEnableOption false;
    # writeConfig = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.awesome = {
      enable = true;
      package = pkgs.awesome-git;
      luaModules = with pkgs; [
        luajitPackages.fennel
      ];
    };

    modules.desktop.lightdm = {
      enable = true;
      defaultSession = "none+awesome";
    };

    home.configFile."awesome" = {
      source = "${config.dotfiles.configDir}/awesome";
      target = "./awesome";
      recursive = true;
    };

    # TODO statically generated theme information that awesome can load
    # home.file."globals.json" = {
    #   text =
    #     builtins.toJSON {
    #     };
    # };
  };
}
