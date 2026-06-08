{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.btop;
  colors = config.modules.themes.colors.withHashtag;

  themeName = "base16";

  themeText = with colors; ''
    theme[main_bg]="${base00}"
    theme[main_fg]="${base05}"
    theme[title]="${base05}"
    theme[hi_fg]="${base0D}"
    theme[selected_bg]="${base02}"
    theme[selected_fg]="${base0D}"
    theme[inactive_fg]="${base03}"
    theme[graph_text]="${base0D}"
    theme[meter_bg]="${base01}"
    theme[proc_misc]="${base0C}"
    theme[cpu_box]="${base0D}"
    theme[mem_box]="${base0B}"
    theme[net_box]="${base0E}"
    theme[proc_box]="${base08}"
    theme[div_line]="${base02}"
    theme[temp_start]="${base0B}"
    theme[temp_mid]="${base0A}"
    theme[temp_end]="${base08}"
    theme[cpu_start]="${base0B}"
    theme[cpu_mid]="${base0A}"
    theme[cpu_end]="${base08}"
    theme[free_start]="${base0D}"
    theme[free_mid]="${base0C}"
    theme[free_end]="${base0E}"
    theme[cached_start]="${base0C}"
    theme[cached_mid]="${base0D}"
    theme[cached_end]="${base0E}"
    theme[available_start]="${base0A}"
    theme[available_mid]="${base08}"
    theme[available_end]="${base0F}"
    theme[used_start]="${base0B}"
    theme[used_mid]="${base0A}"
    theme[used_end]="${base08}"
    theme[download_start]="${base0B}"
    theme[download_mid]="${base0A}"
    theme[download_end]="${base08}"
    theme[upload_start]="${base0D}"
    theme[upload_mid]="${base0C}"
    theme[upload_end]="${base0E}"
    theme[process_start]="${base0B}"
    theme[process_mid]="${base0A}"
    theme[process_end]="${base08}"
  '';
in {
  options.modules.programs.btop = {
    enable = mkEnableOption false;
    vimKeys = mkOption {
      description = "Enable vim-style navigation keys (h/j/k/l) in btop.";
      default = true;
      type = types.bool;
    };
  };

  config = mkIf cfg.enable {
    home.programs.btop = {
      enable = true;
      settings = {
        color_theme = themeName;
        theme_background = false;
        vim_keys = cfg.vimKeys;
        update_ms = 1000;
      };
    };

    home.configFile."btop/themes/${themeName}.theme".text = themeText;
  };
}
