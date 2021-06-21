let colors = import ../colors.nix;
in {
  type = "internal/bspwm";

  ws-icon-0 = "I;1";
  ws-icon-1 = "II;2";
  ws-icon-2 = "III;3";
  ws-icon-3 = "IV;4";
  ws-icon-4 = "V;5";
  ws-icon-5 = "VI;6";
  ws-icon-6 = "VII;7";
  ws-icon-7 = "VIII;8";
  ws-icon-8 = "IX;9";
  ws-icon-default = "0";

  format = "<label-state> <label-mode>";
  format-padding = "1";

  label-focused = "%{T4}%icon%%{T-}";
  label-focused-foreground = "${colors.blue}";
  label-focused-background = "${colors.grey}";
  label-focused-padding = "1";

  label-occupied = "%{T4}%icon%%{T-}";
  label-occupied-foreground = "${colors.magenta}";
  label-occupied-padding = "1";

  label-urgent = "%{T4}%icon%%{T-}";
  label-urgent-background = "${colors.red}";
  label-urgent-padding = "1";

  label-empty = "%{T4}%icon%%{T-}";
  label-empty-foreground = "${colors.grey}";
  label-empty-padding = "1";

  label-monocle = "%{T3}喝 %{T-}";
  label-monocle-foreground = "${colors.cyan}";
  label-fullscreen = "%{T4} %{T-}";
  label-fullscreen-foreground = "${colors.cyan}";
  label-locked = "%{T3} %{T-}";
  label-locked-foreground = "${colors.yellow}";
  label-sticky = "%{T3}𢡊 %{T-}";
  label-sticky-foreground = "${colors.yellow}";
  label-private = "%{T3}廬 %{T-}";
  label-private-foreground = "${colors.red}";
  label-marked = "%{T3} %{T-}";
  label-marked-foreground = "${colors.green}";
}
