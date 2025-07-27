{colors}:
with colors; {
  type = "internal/date";
  interval = "1";

  time = " %a, %b %d %I:%M:%S %p";
  time-alt = " %I:%M";

  format-padding = "1";
  format-prefix = "%{T3}󰥔%{T-}";
  format-prefix-foreground = "${bright-cyan}";
  label = "%{T2}%time%%{T-}";
  label-foreground = "${base07}";
}
