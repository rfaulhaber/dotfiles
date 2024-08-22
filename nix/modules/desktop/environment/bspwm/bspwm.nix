{
  config,
  lib,
  monitors,
}:
with lib; let
  defaultMonitorValue = ["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X"];
  mkMonitor = monitor: {"${monitor}" = defaultMonitorValue;};
  merge = foldr (n: a: n // a) {};
in {
  monitors = merge (map mkMonitor monitors);
  startupPrograms = ["sxhkd" "${config.dotfiles.binDir}/polybar/launch"];
  settings = {
    border_width = 0;
    window_gap = 12;
    split_ratio = 0.52;
    borderless_monocle = true;
    gapless_monocle = true;
    external_rules_command = "${config.dotfiles.binDir}/bspwm/external_rules";
  };
  rules = {
    "Firefox Developer Edition" = {
      desktop = "^1";
      state = "tiled";
      focus = true;
    };
    "Emacs" = {
      desktop = "^2";
      state = "tiled";
      focus = true;
      locked = true;
    };
    kitty = {
      desktop = "^3";
      state = "tiled";
      focus = true;
    };
    discord = {
      desktop = "^4";
      state = "tiled";
    };
    "TelegramDesktop" = {
      desktop = "^4";
      state = "tiled";
    };
    "Spotify" = {
      desktop = "^5";
      state = "tiled";
    };
  };
}
