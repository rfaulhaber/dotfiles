{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.yubikey;
in {
  options.modules.services.yubikey = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    # services.pcscd.enable = true;
    services.udev.packages = with pkgs; [ yubikey-personalization libu2f-host ];

    security.pam.u2f = {
      enable = true;
      settings = {
        # interactive = true;
        cue = true;
        openasuser = true;
        origin = "pam://yubi";
        # the setting in nixpkgs is "authFile" but the setting in the pam-u2f documentation is "authfile"
        # the former does not work while this does
        authfile = pkgs.writeText "u2f-mappings" (lib.concatStrings [
          config.user.name
          ":mEWrvBLxquMeVyb7wN0MQ5+qQobyMQDPm7EbSasf4O7obEgcngY40mq+Tq9gc/IIbIiLkA9p++7vGzb8k1mkpw==,T45SuFOGn+U+wiQ39BQ/xTG3oBHf1JjzEgckVt1mEItFjdDuPf3dHhqM71cylSfueOPh1Ox8x36mgfz0i+hV1g==,es256,+presence"
        ]);
      };
    };
  };
}
