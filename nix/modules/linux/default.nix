{pkgs, lib, ...}: {
  imports = [
    ./oci
    ./services
  ];

  config = {
    # generic set of packages that I use on all my Linux machines
    user.packages = with pkgs; [
      bat
      btop
      fd
      fzf
      pandoc
      ripgrep
      rsync
      unzip
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

    boot.tmp = {
      useTmpfs = lib.mkDefault true;
      cleanOnBoot = lib.mkDefault true;
    };

    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    system.stateVersion = "23.11";
  };
}
