{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.programs.ghostty;
in {
  options.modules.programs.ghostty = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [inputs.ghostty.packages.${pkgs.system}.default];

    home.file.ghostty_config = {
      source = "${config.dotfiles.configDir}/ghostty";
      target = "${config.user.home}/.config/ghostty";
      recursive = true;
    };

    nix.settings = {
      substituters = ["https://ghostty.cachix.org"];
      trusted-public-keys = ["ghostty.cachix.org-1:QB389yTa6gTyneehvqG58y0WnHjQOqgnA+wBnpWWxns="];
    };
  };
}
