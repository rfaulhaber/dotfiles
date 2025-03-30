# linux system defaults
{
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf pkgs.stdenv.targetPlatform.isLinux {
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
  };
}
