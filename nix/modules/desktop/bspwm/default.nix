{ config, lib, pkgs, home-manager, ... }:

with lib;

let cfg = config.modules.desktop.bspwm;
in {
  options.modules.desktop.bspwm = {
    enable = mkOption {
      default = false;
      type = types.bool;
      description = "Enable bspwm";
    };
  };
  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";
      windowManager.bspwm.enable = true;
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+bspwm";
        # TODO move somewhere else!
        # sessionCommands = ''
        #   ~/Projects/dotfiles/nix/hosts/mir3/random-wallpaper.sh
        # '';
      };
      # TODO modularize!
      videoDrivers = [ "nvidia" ];
    };
    # NB: IN ORDER FOR ANY OF THIS TO WORK YOU NEED THIS SET!!
    # I WASTED MOST OF A SUNDAY TRYING TO FIGURE THIS OUT!!!
    # IT SURE WOULD HAVE BEEN GREAT TO KNOW THAT SOMEWHERE!!!!
    home-manager.users.${config.user.name}.xsession.enable = true;

    home.bspwm = {
      enable = true;
      monitors = {
        "DP-0" = [ "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X" ];
      };
      startupPrograms = [ "sxhkd" "$HOME/.config/bin/polybar/launch" ];
      settings = {
        border_width = 0;
        window_gap = 12;
        split_ratio = 0.52;
        borderless_monocle = true;
        gapless_monocle = true;
        external_rules_command = "$HOME/.config/bin/bspwm/external_rules";
      };
      rules = {
        "Firefox Developer Edition" = {
          desktop = "^1";
          state = "tiled";
          focus = true;
        };
        "Emacs" = {
          desktop = "^2";
          state = "tiled";
          focus = true;
          locked = true;
        };
        kitty = {
          desktop = "^3";
          state = "tiled";
          focus = true;
        };
        discord = {
          desktop = "^4";
          state = "tiled";
        };
        "TelegramDesktop" = {
          desktop = "^4";
          state = "tiled";
        };
      };
    };

    home.services.sxhkd = {
      enable = true;

      extraConfig = ''
        #
        # wm independent hotkeys
        #

        # terminal emulator
        super + Return
        	kitty

        super + alt + d
        	rofi -show drun -modi drun,run -show-icons

        super + alt + r
        	rofi -show run -show-icons

        super + alt + w
        	rofi -show window

        super + alt + s
        	gnome-screenshot -i

        super + alt + p
        	  polybar-msg cmd restart


        # make sxhkd reload its configuration files:
        super + Escape
        	pkill -USR1 -x sxhkd

        super + e
        	  emacsclient -c

        super + alt + e
        	  emacsclient -e "(emacs-everywhere)"

        super + b
        	  firefox-devedition

        #
        # bspwm hotkeys
        #

        # quit/restart bspwm
        super + alt + {q,r}
        	bspc {quit,wm -r}

        # close and kill
        super + {_,shift + }w
        	bspc node -{c,k}

        # alternate between the tiled and monocle layout
        super + m
        	bspc desktop -l next

        # send the newest marked node to the newest preselected node
        super + y
        	bspc node newest.marked.local -n newest.!automatic.local

        # swap the current node and the biggest node
        super + g
        	bspc node -s biggest

        #
        # state/flags
        #

        # set the window state
        super + {t,shift + t,s,f}
        	bspc node -t {tiled,pseudo_tiled,floating,fullscreen}

        # set the node flags
        super + ctrl + {m,x,y,z}
        	bspc node -g {marked,locked,sticky,private}

        #
        # focus/swap
        #

        # focus the node in the given direction
        super + {_,shift + }{h,j,k,l}
        	bspc node -{f,s} {west,south,north,east}

        # focus the node for the given path jump
        super + {p,b,comma,period}
        	bspc node -f @{parent,brother,first,second}

        # focus the next/previous node in the current desktop
        super + {_,shift + }c
        	bspc node -f {next,prev}.local

        # focus the next/previous desktop in the current monitor
        super + bracket{left,right}
        	bspc desktop -f {prev,next}.occupied.local

        # focus the last node/desktop
        super + {grave,Tab}
        	bspc {node,desktop} -f last

        # focus the older or newer node in the focus history
        super + {o,i}
        	bspc wm -h off; \
        	bspc node {older,newer} -f; \
        	bspc wm -h on

        # focus or send to the given desktop
        super + {_,shift + }{1-9,0}
        	bspc {desktop -f,node -d} '^{1-9,10}'

        #
        # preselect
        #

        # preselect the direction
        super + ctrl + {h,j,k,l}
        	bspc node -p {west,south,north,east}

        # preselect the ratio
        super + ctrl + {1-9}
        	bspc node -o 0.{1-9}

        # cancel the preselection for the focused node
        super + ctrl + space
        	bspc node -p cancel

        # cancel the preselection for the focused desktop
        super + ctrl + shift + space
        	bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel

        #
        # move/resize
        #

        # expand a window by moving one of its side outward
        super + alt + {h,j,k,l}
        	bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}

        # contract a window by moving one of its side inward
        super + alt + shift + {h,j,k,l}
        	bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}

        # move a floating window
        super + {Left,Down,Up,Right}
        	bspc node -v {-20 0,0 20,0 -20,20 0}

        # lockscreen
        alt + shift + x
        	betterlockscreen -l dim


        #
        # misc
        #

        # change wallpaper
        super + alt + b
        	  ~/Projects/dotfiles/bin/random-wallpaper
        		'';
    };
  };
}
