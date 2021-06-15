let colors = import ../colors.nix;
in {
  type = "internal/memory";
  interval = "1";

  format = "<ramp-used><label>";
  format-padding = "1";
  format-prefix-foreground = "${colors.cyan}";
  label = "%percentage_used%%";
  label-foreground = "${colors.white}";
  label-padding = "1";

  bar-used-empty = "";
  bar-used-fill = "";
  bar-used-indicator = "";
  bar-used-width = "10";
  bar-used-empty-foreground = "${colors.grey}";
  bar-used-indicator-foreground = "${colors.grey}";
  bar-used-foreground-0 = "${colors.cyan}";
  bar-used-foreground-1 = "${colors.dark-cyan}";
  bar-used-foreground-2 = "${colors.green}";
  bar-used-foreground-3 = "${colors.yellow}";
  bar-used-foreground-4 = "${colors.red}";

  ramp-used-0 = "%{T3}﬙%{T-}";
  ramp-used-1 = "%{T3}﬙%{T-}";
  ramp-used-2 = "%{T3}﬙%{T-}";
  ramp-used-3 = "%{T3}﬙%{T-}";
  ramp-used-4 = "%{T3}﬙%{T-}";
  ramp-used-0-foreground = "${colors.dark-cyan}";
  ramp-used-1-foreground = "${colors.cyan}";
  ramp-used-2-foreground = "${colors.green}";
  ramp-used-3-foreground = "${colors.yellow}";
  ramp-used-4-foreground = "${colors.red}";
}
