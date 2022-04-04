{ colors, config, pkgs, ... }:

# TODO do automatically
{
  bspwm = import ./bspwm.nix { inherit colors; };
  cpu = import ./cpu.nix { inherit colors; };
  date = import ./date.nix { inherit colors; };
  memory = import ./memory.nix { inherit colors; };
  pulseaudio = import ./pulseaudio.nix { inherit colors; };
  wttr = import ./wttr.nix { inherit colors config pkgs; };
  battery = import ./battery.nix { inherit colors config pkgs; };
}
