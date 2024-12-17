{colors}:
with colors; {
  type = "internal/memory";
  interval = "1";

  format = "<ramp-used><label>";
  format-padding = "1";
  format-prefix-foreground = "${cyan}";
  label = "%percentage_used%%";
  label-foreground = "${base07}";
  label-padding = "1";

  bar-used-empty = "";
  bar-used-fill = "";
  bar-used-indicator = "";
  bar-used-width = "10";
  bar-used-empty-foreground = "${base01}";
  bar-used-indicator-foreground = "${base01}";
  bar-used-foreground-0 = "${cyan}";
  bar-used-foreground-1 = "${cyan}";
  bar-used-foreground-2 = "${green}";
  bar-used-foreground-3 = "${yellow}";
  bar-used-foreground-4 = "${red}";

  ramp-used-0 = "%{T3}󰘚%{T-}";
  ramp-used-1 = "%{T3}󰘚%{T-}";
  ramp-used-2 = "%{T3}󰘚%{T-}";
  ramp-used-3 = "%{T3}󰘚%{T-}";
  ramp-used-4 = "%{T3}󰘚%{T-}";
  ramp-used-0-foreground = "${cyan}";
  ramp-used-1-foreground = "${cyan}";
  ramp-used-2-foreground = "${green}";
  ramp-used-3-foreground = "${yellow}";
  ramp-used-4-foreground = "${red}";
}
