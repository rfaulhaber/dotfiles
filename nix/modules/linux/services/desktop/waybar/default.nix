{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.waybar;
  colors = config.modules.themes.colors;
  font = config.modules.themes.font;
  c = colors.withHashtag;

  # Generate theme.scss with the same variables as globals.nix
  themeScss = let
    inherit (builtins) concatStringsSep map;
    inherit (lib) attrsToList;
    theme =
      {
        inherit (c) base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base0A base0B base0C base0D base0E base0F;
        background = c.base00;
        foreground = c.base05;
        inherit (c) red green yellow blue cyan magenta;
      }
      // optionalAttrs (c ? bg) {
        inherit (c) bg bg-alt fg fg-alt grey teal violet orange;
      }
      // optionalAttrs (c ? dark-cyan) {
        inherit (c) dark-cyan dark-blue;
      }
      // optionalAttrs (c ? bright-black) {
        inherit (c) bright-black bright-white;
      };
    colorVars =
      theme
      |> attrsToList
      |> (map ({
        name,
        value,
      }: "\$${name}: ${value};"))
      |> (concatStringsSep "\n");
  in
    colorVars + "\n\$font-family: ${font};\n";

  # Reference the SCSS source as a Nix path so it's copied to the store
  waybarScssSource = ../../../../../../config/waybar/style.scss;

  # Compile style.scss with dart-sass at build time
  waybarStyle = pkgs.runCommand "waybar-style-css" {
    nativeBuildInputs = [pkgs.dart-sass];
  } ''
    # Create a working directory with theme.scss
    mkdir -p $out theme
    echo ${escapeShellArg themeScss} > theme/theme.scss
    # Compile style.scss, using theme/ as a load path for @use "theme.scss"
    sass --no-source-map \
      --load-path=theme \
      ${waybarScssSource} \
      $out/style.css
  '';

  # Waybar JSON config as a Nix attrset
  waybarConfig = {
    height = 45;
    layer = "top";
    modules-left = ["niri/workspaces" "wlr/taskbar" "niri/window"];
    modules-center = [];
    modules-right = ["tray" "cpu" "memory" "disk" "disk#nix" "clock"];
    "wlr/taskbar" = {
      format = "{icon}";
      tooltip-format = "{title} | {app_id}";
      on-click = "activate";
      on-click-middle = "close";
      icon-size = 28;
    };
    clock = {
      format = "󰥔 {:%a, %b %d %Y %I:%M:%S %p}";
      interval = 1;
      tooltip-format = "<span size='10pt'>{calendar}</span>";
      actions = {
        on-click = "mode";
        on-scroll-up = "shift_up";
        on-scroll-down = "shift_down";
        on-right-click = "shift_reset";
      };
      calendar.format = {
        months = "<span color='${c.green}'><b>{}</b></span>";
        days = "<span color='${c.base07}'><b>{}</b></span>";
        weeks = "<span color='${c.cyan}'><b>W{}</b></span>";
        weekdays = "<span color='${c.magenta}'><b>{}</b></span>";
        today = "<span color='${c.blue}'><b><u>{}</u></b></span>";
      };
    };
    cpu = {
      interval = 1;
      format = " {usage}%";
      states = {
        low = 10;
        low-medium = 35;
        medium = 50;
        high = 70;
        max = 90;
      };
    };
    memory = {
      interval = 1;
      format = "󰘚 {percentage}% ({used}/{avail}/{total})";
      states = {
        low = 10;
        low-medium = 35;
        medium = 50;
        high = 70;
        max = 90;
      };
    };
    disk = {
      interval = 30;
      format = "󰋊 {path}: {used}";
      path = "${config.user.home}";
      tooltip = true;
    };
    "disk#nix" = {
      interval = 30;
      format = "󰋊 {path}: {used}";
      path = "/nix";
      tooltip = true;
    };
  };
in {
  options.modules.desktop.waybar = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = [
      inputs.waybar.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];

    home.configFile = {
      "waybar/config".text = builtins.toJSON waybarConfig;
      "waybar/style.css".source = "${waybarStyle}/style.css";
    };
  };
}
