{colors}: with colors; {
  type = "internal/cpu";
  interval = "1";

  format = "<ramp-load><label>";
  format-padding = "1";
  format-prefix-foreground = "${cyan}";
  label = "%percentage:2%%";
  label-foreground = "${base07}";
  label-padding = "1";

  bar-load-empty = "";
  bar-load-fill = "";
  bar-load-indicator = "";
  bar-load-width = "10";
  bar-load-empty-foreground = "${cyan}";
  bar-load-indicator-foreground = "${cyan}";
  bar-load-foreground-0 = "${base07}";
  bar-load-foreground-1 = "${cyan}";
  bar-load-foreground-2 = "${green}";
  bar-load-foreground-3 = "${yellow}";
  bar-load-foreground-4 = "${red}";

  ramp-load-0 = "%{T3}%{T-}";
  ramp-load-1 = "%{T3}%{T-}";
  ramp-load-2 = "%{T3}%{T-}";
  ramp-load-3 = "%{T3}%{T-}";
  ramp-load-4 = "%{T3}%{T-}";
  ramp-load-5 = "%{T3}%{T-}";
  ramp-load-0-foreground = "${base07}";
  ramp-load-1-foreground = "${cyan}";
  ramp-load-2-foreground = "${green}";
  ramp-load-3-foreground = "${yellow}";
  ramp-load-4-foreground = "${orange}";
  ramp-load-5-foreground = "${red}";
}
