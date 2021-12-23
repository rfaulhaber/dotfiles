{ config, pkgs, ... }:
# TODO: assert that all of these programs are installed somehow
let
  kittyExec = "${pkgs.kitty}/bin/kitty";
  rofiExec = "${pkgs.rofi}/bin/rofi";
  bspcExec = "${pkgs.bspwm}/bin/bspc";
  polybarMsgExec = "${pkgs.polybar}/bin/polybar-msg";
  emacsclientExec = "${pkgs.emacs}/bin/emacsclient";
  firefoxExec = "${pkgs.firefox-devedition-bin}/bin/firefox-devedition";
in {
  # terminal
  "super + Return" = kittyExec;
  #rofu apps
  "super + alt + d" = "${rofiExec} -show drun -modi drun,run -show-icons";
  # rofi run
  "super + alt + r" = "${rofiExec} -show run -show-icons";
  # rofi window
  "super + alt + w" = "${rofiExec} -show window";
  # screenshot
  "super + alt + s" = "${pkgs.gnome3.gnome-screenshot}/bin/gnome-screenshot -i";
  # restart polybar
  "super + alt + p" = "${polybarMsgExec} cmd restart";

  "super + Escape" = "pkill -USR1 -x sxhkd";

  "super + e" = "${emacsclientExec} -c";

  "super + alt + e" = "${emacsclientExec} -e '(emacs-everywhere)'";
  "super + b" = firefoxExec;

  #
  # bspwm hotkeys
  #

  # quit/restart bspwm
  "super + alt + {q,r}" = "${bspcExec} {quit,wm -r}";

  # close and kill
  "super + {_,shift + }w" = "${bspcExec} node -{c,k}";

  # alternate between the tiled and monocle layout
  "super + m" = "${bspcExec} desktop -l next";

  # send the newest marked node to the newest preselected node
  "super + y" =
    "${bspcExec} node newest.marked.local -n newest.!automatic.local";
  # swap with biggest window
  "super + g" = "${bspcExec} node --swap biggest";
  # change window state
  "super + {t,shift + t,s,f}" =
    "${bspcExec} node -t {tiled,pseudo_tiled,floating,fullscreen}";
  # mark, lock, sticky, or private
  "super + ctrl + {m,x,y,z}" =
    "${bspcExec} node -g {marked,locked,sticky,private}";
  # motions
  "super + {_,shift + }{h,j,k,l}" =
    "${bspcExec} node -{f,s} {west,south,north,east}";
  "super + {p,b,comma,period}" =
    "${bspcExec} node -f @{parent,brother,first,second}";
  "super + {_,shift + }c" = "${bspcExec} node -f {next,prev}.local";
  # cycle active desktops
  "super + bracket{left,right}" =
    "${bspcExec} desktop -f {prev,next}.occupied.local";
  # cycle previous desktops
  "super + {grave,Tab}" = "${bspcExec} {node,desktop} -f last";
  "super + {o,i}" = ''
    ${bspcExec} wm -h off; \
    ${bspcExec} node {older,newer} -f; \
    ${bspcExec} wm -h on
  '';

  # change desktop
  "super + {_,shift + }{1-9,0}" =
    "${bspcExec} {desktop -f,node -d} '^{1-9,10}'";

  #
  # preselect
  #
  # preselect the direction
  "super + ctrl + {h,j,k,l}" = "${bspcExec} node -p {west,south,north,east}";

  # preselect the ratio
  "super + ctrl + {1-9}" = "${bspcExec} node -o 0.{1-9}";

  # cancel the preselection for the focused node
  "super + ctrl + space" = "${bspcExec} node -p cancel";

  # cancel the preselection for the focused desktop
  "super + ctrl + shift + space" =
    "${bspcExec} query -N -d | xargs -I id -n 1 ${bspcExec} node id -p cancel";

  #
  # move/resize
  #

  # expand a window by moving one of its side outward
  "super + alt + {h,j,k,l}" =
    "${bspcExec} node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}";

  # contract a window by moving one of its side inward
  "super + alt + shift + {h,j,k,l}" =
    "${bspcExec} node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}";

  # move a floating window
  "super + {Left,Down,Up,Right}" =
    "${bspcExec} node -v {-20 0,0 20,0 -20,20 0}";

  # lockscreen
  "alt + shift + x" = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";

  #
  # misc
  #

  # change wallpaper
  #"super + alt + b" = "~/Projects/dotfiles/bin/random-wallpaper";
  "super + alt + b" = "${config.dotfiles.binDir}/random-wallpaper";
}
