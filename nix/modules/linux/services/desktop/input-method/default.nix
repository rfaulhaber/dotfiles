{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.inputMethod;
  inherit (config.modules.desktop) keyboard;

  # fcitx5 spells an xkb layout/variant pair as "<layout>-<variant>".
  layout = keyboard.layout + optionalString (keyboard.variant != "") "-${keyboard.variant}";

  # nixpkgs installs rime-ice's default.yaml as rime_ice_suggestion.yaml and
  # leaves an empty default.yaml behind, so nothing lists a schema until this
  # patch includes it back. Rime's own ascii-mode keys are unbound: Rime keeps
  # reporting itself as the current input method while in ascii mode, so the
  # bar would read 中 over English. Leaving Chinese goes through fcitx5 instead.
  rimeSettings = pkgs.writeTextDir "share/rime-data/default.custom.yaml" ''
    patch:
      __include: rime_ice_suggestion:/
      ascii_composer:
        switch_key:
          Caps_Lock: noop
          Shift_L: noop
          Shift_R: noop
  '';

  fcitx5-rime = pkgs.fcitx5-rime.override {rimeDataPkgs = [rimeSettings pkgs.rime-ice];};
  rimeData = fcitx5-rime.rimeDataDrv;
in {
  options.modules.desktop.inputMethod = {
    enable = mkEnableOption false;

    layoutInputMethod = mkOption {
      type = types.str;
      default = "keyboard-${layout}";
      readOnly = true;
      description = ''
        fcitx5's name for the plain keyboard entry, which is what
        `fcitx5-remote -n` prints while nothing is composing. Exposed so the
        bar can label it.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.desktop.environment.isWayland;
        message = "The input method module relies on the Wayland input-method protocol";
      }
    ];

    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        # Apps reach fcitx5 through the compositor's text-input-v3 rather than
        # a toolkit IM module, so GTK_IM_MODULE/QT_IM_MODULE stay unset.
        waylandFrontend = true;
        addons = [fcitx5-rime];

        settings.globalOptions = {
          "Hotkey/TriggerKeys"."0" = "Super+space";
          # Shift_L by default: a second way out of Rime that bypasses the
          # keybind, so the bar would lag behind it.
          "Hotkey/AltTriggerKeys" = {};
          # Both default to Super(+Shift)+space. There is only one group.
          "Hotkey/EnumerateGroupForwardKeys" = {};
          "Hotkey/EnumerateGroupBackwardKeys" = {};
          # One state for the whole desktop, so the bar describes whichever
          # window takes the next keystroke.
          Behavior.ShareInputState = "All";
        };

        # The first item is what toggling off returns to.
        settings.inputMethod = {
          GroupOrder."0" = "Default";
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = layout;
            DefaultIM = "rime";
          };
          "Groups/0/Items/0".Name = cfg.layoutInputMethod;
          "Groups/0/Items/1".Name = "rime";
        };
      };
    };

    # fcitx5 rewrites its profile on exit and on idle autosave, and the first
    # copy in ~/.config shadows /etc/xdg from then on. Linking the user paths
    # back to the generated files leaves them read-only; fcitx5's save then
    # fails silently and the next start reads these again.
    home.configFile = {
      "fcitx5/profile".source = config.environment.etc."xdg/fcitx5/profile".source;
      "fcitx5/config".source = config.environment.etc."xdg/fcitx5/config".source;
    };

    # Rime compiles its data into build/ under its user directory and decides
    # what to recompile by comparing source mtimes, which are all equal in the
    # store. A changed schema or patch would never reach the compiled copy, so
    # drop it whenever the data changes; fcitx5 recompiles on its next start.
    home-manager.users.${config.user.name} = {
      config,
      lib,
      ...
    }: {
      home.activation.rimeData = lib.hm.dag.entryAfter ["writeBoundary"] ''
        rimeDir="${config.xdg.dataHome}/fcitx5/rime"
        if [[ -d $rimeDir/build && $(readlink "$rimeDir/.nix-rime-data" || true) != ${rimeData} ]]; then
          run rm -rf "$rimeDir/build"
        fi
        run mkdir -p "$rimeDir"
        run ln -sfn ${rimeData} "$rimeDir/.nix-rime-data"
      '';
    };
  };
}
