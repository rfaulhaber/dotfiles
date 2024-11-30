{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [../base.nix];

  environment.systemPackages = with pkgs; [
    inputs.disko.packages.x86_64-linux.disko
  ];
}
