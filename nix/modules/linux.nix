# linux system defaults
{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf pkgs.stdenv.targetPlatform.isLinux {
    environment.systemPackages = with pkgs; [
      bat
      binutils
      coreutils-full
      curl
      fd
      fzf
      htop
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
