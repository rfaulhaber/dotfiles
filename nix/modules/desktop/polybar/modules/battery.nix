{ config, pkgs, ... }:

# TODO make look nicer!
# https://github.com/polybar/polybar/wiki/Module%3A-battery
{
  type = "internal/battery";
  battery = "BAT0";
  adapter = "AC";
  low-at = "10";

  format-charging = "<animation-charging> <label-charging>";
  format-discharging = "<ramp-capacity> <label-discharging>";
  format-full = "<ramp-capacity> <label-full>";

  ramp-capacity-0 = "";
  ramp-capacity-1 = "";
  ramp-capacity-2 = "";
  ramp-capacity-3 = "";
  ramp-capacity-4 = "";

  bar-capacity-width = "10";

  animation-charging-0 = "";
  animation-charging-1 = "";
  animation-charging-2 = "";
  animation-charging-3 = "";
  animation-charging-4 = "";
  animation-charging-framerate = "750";

  animation-discharging-0 = "";
  animation-discharging-1 = "";
  animation-discharging-2 = "";
  animation-discharging-3 = "";
  animation-discharging-4 = "";
  animation-discharging-framerate = "500";
}
