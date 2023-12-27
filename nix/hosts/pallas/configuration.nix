# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../../modules
  ];

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      docker.enable = true;
      gpg.enable = true;
      systemd.modules = {
        dockerCleanup.enable = true;
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBvwfQy4U/GVd5S2JhNnaQvuKizbavuUWihmr/89fjZo ryan@hyperion"
          ];
          port = 11689;
        };
      };
    };
  };
}
