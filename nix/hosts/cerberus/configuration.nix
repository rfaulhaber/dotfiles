# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [../../modules];

  # only needed for crosscompilation
  nixpkgs.crossSystem = lib.systems.elaborate lib.systems.examples.aarch64-multiplatform;

  sdImage.postBuildCommands = with pkgs; ''
    dd if=${ubootRock64}/idbloader.img of=$img conv=fsync,notrunc bs=512 seek=64
    dd if=${ubootRock64}/u-boot.itb of=$img conv=fsync,notrunc bs=512 seek=16384
  '';

  modules = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
        ohMyZsh = {
          enable = true;
          theme = "agnoster";
        };
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      docker.enable = true;
      gpg.enable = true;
      systemd.modules = with lib.my.systemdModules; [updatedb dockerCleanup];
      ssh = {
        enable = true;
        server = {
          enable = true;
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBvwfQy4U/GVd5S2JhNnaQvuKizbavuUWihmr/89fjZo ryan@hyperion"
          ];
        };
      };
    };
    themes.active = "city-lights";
  };

  networking = {
    hostName = "cerberus";
  };
}
