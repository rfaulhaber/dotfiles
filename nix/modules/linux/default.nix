{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./desktop
    ./docker
    ./hardware
    ./mullvad
    ./netbird
    ./printing
    ./redshift
    ./samba-mount
    ./samba-serve
    ./sudo-rs
    ./syncthing
    ./systemd
    ./themes
    ./yubikey
    ./zerotier
    ./zfs
  ];

  config = {
    user.packages = with pkgs; [
      bat
      binutils
      coreutils-full
      curl
      fd
      fzf
      btop
      pandoc
      ripgrep
      rsync
      tokei
      unzip
      wget
      zip
    ];

    # All my machines are in the same timezone
    # TODO should this be here?

    time.timeZone = "America/New_York";

    # TODO where should these live?
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    system.stateVersion = "23.11";
  };
}
