{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.niri;
  colors = config.modules.themes.colors;
  # The host must wire inputs.niri.overlays.default into nixpkgs.overlays
  # (see flake.nix). Going through pkgs rather than inputs.niri.packages —
  # which is built against bare nixpkgs.legacyPackages — lets host overlays
  # reach niri's dependencies. Without the overlay this silently falls back
  # to nixpkgs' older niri.
  niriPkg = pkgs.niri;
in {
  imports = [
    ../../swww
    ../../wayland
    inputs.niri-flake.nixosModules.niri
  ];

  options.modules.desktop.environment.niri = {
    enable = mkEnableOption false;
    outputs = mkOption {
      description = ''
        Per-output Niri display configuration, forwarded verbatim to
        `programs.niri.settings.outputs`. Keyed by connector name
        (e.g. "DP-0", "eDP-1"). See the niri-flake HM module for the
        accepted shape (`mode`, `scale`, `transform`, `position`, etc.).
      '';
      type = types.attrsOf (types.attrsOf types.anything);
      default = {};
      example = literalExpression ''
        {
          "DP-0" = {
            mode = { width = 2560; height = 1440; refresh = 60.0; };
            position = { x = 0; y = 0; };
          };
          "DP-1" = {
            mode = { width = 3840; height = 2160; refresh = 59.997; };
            position = { x = 2560; y = 0; };
          };
        }
      '';
    };
  };

  config = mkIf cfg.enable {
    nix.settings = {
      substituters = ["https://niri.cachix.org"];
      trusted-public-keys = ["niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="];
    };

    modules.desktop = {
      swww.enable = true;
      wayland.enable = true;
      waybar.enable = true;
      environment.type = "wayland";
      fuzzel.enable = true;
    };

    systemd.packages = [niriPkg];

    # switch-to-configuration restarts changed *user* units, and any rebuild of
    # niri's closure changes niri.service's ExecStart store path (even at the
    # same niri version) — restarting it kills the whole graphical session.
    # Keep the running compositor; the new build takes over at next login.
    # Written at the units level (not systemd.user.services.niri.restartIfChanged)
    # because serviceToUnit would also inject Environment=PATH into the drop-in,
    # clobbering the session PATH niri uses to spawn waybar/fuzzel/etc.
    systemd.user.units."niri.service".text = ''
      [Service]
      X-RestartIfChanged=false
    '';

    security.polkit.enable = true;

    programs = {
      niri = {
        enable = true;
        package = niriPkg;
      };

      xwayland.enable = true;
    };

    services = {
      gnome.gnome-keyring.enable = true;
      greetd = {
        enable = true;
        settings.default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --cmd '${niriPkg}/bin/niri-session'";
          user = "greeter";
        };
      };
    };

    user.packages = with pkgs; [
      fuzzel
      swaylock
    ];

    environment.systemPackages = with pkgs; [
      xdg-desktop-portal-gtk
      xwayland-satellite
      nautilus
    ];

    # niri settings via the sodiboo/niri-flake home-manager module
    # (nixosModules.niri auto-imports the HM module when home-manager is present)
    home-manager.users.${config.user.name}.programs.niri.settings = {
      input = {
        keyboard.xkb = {};
        touchpad = {
          tap = true;
          natural-scroll = true;
        };
      };

      outputs = cfg.outputs;

      layout = import ./layout.nix {inherit colors;};

      spawn-at-startup = [
        {argv = ["waybar"];}
        {argv = ["xwayland-satellite"];}
      ];

      environment = {
        DISPLAY = ":0";
      };

      screenshot-path = "~/pictures/screenshots/screenshot-%Y-%m-%d-%H:%M:%S.png";

      window-rules = import ./window-rules.nix;
      binds = import ./binds.nix {inherit config lib pkgs;};
    };
  };
}
