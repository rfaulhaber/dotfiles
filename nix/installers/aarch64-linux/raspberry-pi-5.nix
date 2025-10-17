{
  inputs,
  modulesPath,
  ...
}: {
  imports = with inputs.nixos-raspberrypi.nixosModules; [
    ../base.nix
    raspberry-pi-5.base
  ];

  disabledModules = [
    (modulesPath + "/rename.nix")
  ];

  networking.hostName = "nixos";
}
