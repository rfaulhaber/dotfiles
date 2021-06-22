{ config, lib, pkgs, ... }:

{
  config = {
    # bare minimum required packages
    environment.systemPackages = with pkgs; [
      bat
      coreutils-full
      croc
      curl
      exa
      fd
      fzf
      htop
      jq
      pandoc
      ripgrep
      rsync
      tokei
      unzip
      wget
      zip
      zoxide
    ];
  };
}
