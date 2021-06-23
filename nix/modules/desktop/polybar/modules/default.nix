{ colors, ... }:

{
  bspwm = import ./bspwm.nix { inherit colors; };
  cpu = import ./cpu.nix { inherit colors; };
  date = import ./date.nix { inherit colors; };
  memory = import ./memory.nix { inherit colors; };
  pulseaudio = import ./pulseaudio.nix { inherit colors; };
  wttr = import ./wttr.nix { inherit colors; };
}
