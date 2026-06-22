{
  config,
  lib,
  pkgs,
  isLinux,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types mkIf mkMerge optionals;
  isX11 = isLinux && config.modules.desktop.environment.isX11;
  isWayland = isLinux && config.modules.desktop.environment.isWayland;
  cfg = config.modules.programs.nushell;
  desktopCfg = config.modules.desktop;
in {
  options.modules.programs.nushell = {
    enable = mkEnableOption false;
    setDefault = mkOption {
      description = "Sets Nushell to be the default shell for the system user.";
      default = false;
      type = types.bool;
    };
    zoxide = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
    carapace = {
      enable = mkOption {
        type = types.bool;
        default = true;
      };
    };
    plugins = mkOption {
      type = types.listOf types.package;
      description = "Nushell plugins to include.";
      default = [];
    };
  };

  config = mkIf cfg.enable {
    # TODO import config into nu configuration from here
    home.programs = {
      nushell = let
        # Source the static nushell config from its own content-addressed store
        # path rather than the ~/.config/dotfiles mirror. config.nu/env.nu source
        # their siblings with relative paths (`source "./hosts/..."`, `use
        # themes`), which resolve within this store dir since the whole
        # config/nushell subtree is imported together.
        configDir = builtins.path {
          path = "${config.dotfiles.configDir}/nushell";
          name = "nushell-config";
        };
      in {
        enable = true;

        # given the way nushell loads, this causes all the sourced files in the
        # dotfiles to not load correctly. so the way we avoid that is to have
        # home manager's config load the config from the dotfiles like so.
        # a generated theme file is sourced after to override the hardcoded
        # tokyo-night fallback with colors from the active system theme.
        configFile.text = ''
          source ${configDir}/config.nu
          source ${config.user.home}/.config/nushell/generated-theme.nu
        '';
        envFile.text = "source ${configDir}/env.nu";

        shellAliases = mkIf (pkgs.stdenv.isLinux && desktopCfg.enable) (mkMerge [
          (mkIf isWayland {
            pbcopy = "${pkgs.wl-clipboard}/bin/wl-copy";
            pbpaste = "${pkgs.wl-clipboard}/bin/wl-paste";
          })
          (mkIf isX11 {
            pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
            pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
          })
        ]);

        plugins = cfg.plugins;
      };

      zoxide.enable = cfg.zoxide.enable;

      # TODO there is an issue where if carapace is not enabled the
      # configuration cannot load correctly. carapace should be optional
      carapace.enable = cfg.carapace.enable;
    };

    # generate a theme override using the active system theme
    home.file.nushellGeneratedTheme = let
      nushellConfigs = import ../../../lib/configs/nushell.nix {
        colors = config.modules.themes.colors.withHashtag;
        themeName = config.modules.themes.colors.scheme;
      };
    in {
      target = "${config.user.home}/.config/nushell/generated-theme.nu";
      text = nushellConfigs.generated-theme;
    };

    user.shell = mkIf cfg.setDefault pkgs.nushell;

    environment.shells = [pkgs.nushell];

    user.packages =
      [
        pkgs.bat # bat is used as nushell's pager. see config/nushell/env.nu
      ]
      ++ lib.optional isX11 pkgs.xclip
      ++ lib.optional isWayland pkgs.wl-clipboard;
  };
}
