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
        configDir = "${config.home.file.dotfiles.target}/config/nushell";
      in {
        enable = true;

        # given the way nushell loads, this causes all the sourced files in the
        # dotfiles to not load correctly. so the way we avoid that is to have
        # home manager's config load the config from the dotfiles like so.
        # on Linux, we also source a generated theme file that overrides the
        # hardcoded tokyo-night theme with colors from the active system theme.
        configFile.text =
          "source ${configDir}/config.nu"
          + lib.optionalString isLinux
          "\nsource ${config.user.home}/.config/nushell/generated-theme.nu";
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

    # generate a theme override on Linux using the active system theme
    home.file.nushellGeneratedTheme = mkIf isLinux (let
      c = config.modules.themes.colors.withHashtag;
    in {
      target = "${config.user.home}/.config/nushell/generated-theme.nu";
      text = ''
        # Auto-generated from system theme (${config.modules.themes.colors.scheme}).
        # Do not edit — changes will be overwritten on rebuild.
        $env.config.color_config = {
            binary: '${c.magenta}'
            block: '${c.blue}'
            cell-path: '${c.fg}'
            closure: '${c.dark-cyan}'
            custom: '${c.fg-alt}'
            duration: '${c.yellow}'
            float: '${c.red}'
            glob: '${c.fg-alt}'
            int: '${c.magenta}'
            list: '${c.dark-cyan}'
            nothing: '${c.red}'
            range: '${c.yellow}'
            record: '${c.dark-cyan}'
            string: '${c.green}'

            bool: {|| if $in { '${c.dark-cyan}' } else { '${c.yellow}' } }

            date: {|| (date now) - $in |
                if $in < 1hr {
                    { fg: '${c.red}' attr: 'b' }
                } else if $in < 6hr {
                    '${c.red}'
                } else if $in < 1day {
                    '${c.yellow}'
                } else if $in < 3day {
                    '${c.green}'
                } else if $in < 1wk {
                    { fg: '${c.green}' attr: 'b' }
                } else if $in < 6wk {
                    '${c.dark-cyan}'
                } else if $in < 52wk {
                    '${c.blue}'
                } else { 'dark_gray' }
            }

            filesize: {|e|
                if $e == 0b {
                    '${c.fg}'
                } else if $e < 1mb {
                    '${c.dark-cyan}'
                } else {{ fg: '${c.blue}' }}
            }

            shape_and: { fg: '${c.magenta}' attr: 'b' }
            shape_binary: { fg: '${c.magenta}' attr: 'b' }
            shape_block: { fg: '${c.blue}' attr: 'b' }
            shape_bool: '${c.dark-cyan}'
            shape_closure: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_custom: '${c.green}'
            shape_datetime: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_directory: { fg: '${c.green}' attr: 'b' }
            shape_external: '${c.dark-cyan}'
            shape_external_resolved: { fg: '${c.yellow}' attr: 'b' }
            shape_externalarg: { fg: '${c.green}' attr: 'b' }
            shape_filepath: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_flag: { fg: '${c.blue}' attr: 'b' }
            shape_float: { fg: '${c.red}' attr: 'b' }
            shape_garbage: { fg: '#FFFFFF' bg: '#FF0000' attr: 'b' }
            shape_glob_interpolation: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_globpattern: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_int: { fg: '${c.magenta}' attr: 'b' }
            shape_internalcall: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_keyword: { fg: '${c.magenta}' attr: 'b' }
            shape_list: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_literal: '${c.blue}'
            shape_match_pattern: '${c.green}'
            shape_matching_brackets: { attr: 'u' }
            shape_nothing: '${c.red}'
            shape_operator: '${c.yellow}'
            shape_or: { fg: '${c.magenta}' attr: 'b' }
            shape_pipe: { fg: '${c.magenta}' attr: 'b' }
            shape_range: { fg: '${c.yellow}' attr: 'b' }
            shape_raw_string: { fg: '${c.fg-alt}' attr: 'b' }
            shape_record: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_redirection: { fg: '${c.magenta}' attr: 'b' }
            shape_signature: { fg: '${c.green}' attr: 'b' }
            shape_string: '${c.green}'
            shape_string_interpolation: { fg: '${c.dark-cyan}' attr: 'b' }
            shape_table: { fg: '${c.blue}' attr: 'b' }
            shape_vardecl: { fg: '${c.blue}' attr: 'u' }
            shape_variable: '${c.magenta}'

            foreground: '${c.fg-alt}'
            background: '${c.bg}'
            cursor: '${c.fg-alt}'

            empty: '${c.blue}'
            header: { fg: '${c.green}' attr: 'b' }
            hints: '${c.bright-blue}'
            leading_trailing_space_bg: { attr: 'n' }
            row_index: { fg: '${c.green}' attr: 'b' }
            search_result: { fg: '${c.red}' bg: '${c.fg}' }
            separator: '${c.fg}'
        }
      '';
    });

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
