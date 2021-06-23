{ colors }:

{
  type = "internal/cpu";
  interval = "1";

  format = "<ramp-load><label>";
  format-padding = "1";
  format-prefix-foreground = "${colors.cyan}";
  label = "%percentage:2%%";
  label-foreground = "${colors.white}";
  label-padding = "1";

  bar-load-empty = "";
  bar-load-fill = "";
  bar-load-indicator = "";
  bar-load-width = "10";
  bar-load-empty-foreground = "${colors.cyan}";
  bar-load-indicator-foreground = "${colors.cyan}";
  bar-load-foreground-0 = "${colors.dark-cyan}";
  bar-load-foreground-1 = "${colors.cyan}";
  bar-load-foreground-2 = "${colors.green}";
  bar-load-foreground-3 = "${colors.yellow}";
  bar-load-foreground-4 = "${colors.red}";

  ramp-load-0 = "%{T3}%{T-}";
  ramp-load-1 = "%{T3}%{T-}";
  ramp-load-2 = "%{T3}%{T-}";
  ramp-load-3 = "%{T3}%{T-}";
  ramp-load-4 = "%{T3}%{T-}";
  ramp-load-5 = "%{T3}%{T-}";
  ramp-load-0-foreground = "${colors.dark-cyan}";
  ramp-load-1-foreground = "${colors.cyan}";
  ramp-load-2-foreground = "${colors.green}";
  ramp-load-3-foreground = "${colors.yellow}";
  ramp-load-4-foreground = "${colors.orange}";
  ramp-load-5-foreground = "${colors.red}";
}
