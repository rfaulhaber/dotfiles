{
  config,
  lib,
  pkgs,
  ...
}: {
  config = {
    # bare minimum required packages
    environment.systemPackages = with pkgs; [
      bat
      binutils
      coreutils-full
      croc
      curl
      fd
      fzf
      htop
      jq
      pandoc
      pass
      ripgrep
      rsync
      tokei
      unzip
      wget
      zip
    ];
  };
}
