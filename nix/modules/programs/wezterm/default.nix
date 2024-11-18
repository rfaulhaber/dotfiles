{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.programs.wezterm;
in {
  options.modules.programs.wezterm = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    user.packages = [inputs.wezterm.packages.${pkgs.system}.default];

    # NOTE: using home-manager for wezterm creates a single wezterm.lua file
    # this is undesirable because of my wezterm setup. instead we just link my
    # wezterm config here
    home.file.wezterm_config = {
      source = "${config.dotfiles.configDir}/wezterm";
      target = "${config.user.home}/.config/wezterm";
      recursive = true;
    };

    # allows us to use the cached version of wezterm for the wezterm input
    nix.settings = {
      substituters = ["https://wezterm.cachix.org"];
      trusted-public-keys = ["wezterm.cachix.org-1:kAbhjYUC9qvblTE+s7S+kl5XM1zVa4skO+E/1IDWdH0="];
    };
  };
}
