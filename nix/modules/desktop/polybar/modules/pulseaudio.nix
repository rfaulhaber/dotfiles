{colors}: {
  type = "internal/pulseaudio";
  use-ui-max = "false";

  format-volume = "<ramp-volume> <label-volume>";
  format-volume-padding = "1";
  label-volume = "%percentage%%";
  label-volume-foreground = "${colors.white}";
  label-volume-padding = "1";

  format-muted-padding = "1";
  label-muted = "%{T3}ﱝ%{T-}";
  label-muted-foreground = "${colors.red}";

  bar-volume-empty = "";
  bar-volume-fill = "";
  bar-volume-indicator = "";
  bar-volume-width = "10";
  # bar-volume-empty-foreground = "${colors.polar-night-4}";
  # bar-volume-indicator-foreground = "${colors.snow-storm-1}";
  bar-volume-foreground-0 = "${colors.dark-cyan}";
  bar-volume-foreground-1 = "${colors.cyan}";
  bar-volume-foreground-2 = "${colors.cyan}";
  bar-volume-foreground-3 = "${colors.yellow}";

  ramp-volume-0 = "%{T3}奄%{T-}";
  ramp-volume-1 = "%{T3}奔%{T-}";
  ramp-volume-2 = "%{T3}%{T-}";
  ramp-volume-0-foreground = "${colors.dark-cyan}";
  ramp-volume-1-foreground = "${colors.cyan}";
  ramp-volume-2-foreground = "${colors.yellow}";
}
