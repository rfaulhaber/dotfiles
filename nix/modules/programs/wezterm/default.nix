{
  config,
  lib,
  pkgs,
  inputs,
  isLinux,
  isDarwin,
  ...
}:
with lib; let
  cfg = config.modules.programs.wezterm;
in {
  options.modules.programs.wezterm = {
    enable = mkEnableOption false;
    useSystemTheme = mkOption {
      description = "Generates a color scheme file based on config.theme.active.";
      type = types.bool;
      default = false;
    };
    weztermPackage = mkOption {
      description = "WezTerm package to use.";
      type = types.package;
      default = inputs.wezterm.packages.${pkgs.system}.default;
    };
  };

  config = mkIf cfg.enable {
    user.packages = [
      cfg.weztermPackage
    ];

    # NOTE: using home-manager for wezterm creates a single wezterm.lua file
    # this is undesirable because of my wezterm setup. instead we just link my
    # wezterm config here
    home.file.wezterm_config = {
      source = "${config.dotfiles.configDir}/wezterm";
      target = "${config.user.home}/.config/wezterm";
      recursive = true;
    };

    environment = let
      vars = {
        WEZTERM_CONFIG_DIR = config.home.file.wezterm_config.target;
        WEZTERM_CONFIG_FILE = "${config.home.file.wezterm_config.target}/wezterm.lua";
      };
    in
      lib.optionalAttrs isDarwin {
        variables = vars;
      }
      // lib.optionalAttrs isLinux {
        sessionVariables = vars;
      };

    home.file.weztermApp = mkIf isDarwin {
      source = "${cfg.weztermPackage}/Applications/WezTerm.app";
      target = "${config.user.home}/Applications/WezTerm.app";
    };

    # allows us to use the cached version of wezterm for the wezterm input
    nix.settings = {
      substituters = ["https://wezterm.cachix.org"];
      trusted-public-keys = ["wezterm.cachix.org-1:kAbhjYUC9qvblTE+s7S+kl5XM1zVa4skO+E/1IDWdH0="];
    };
  };
}
