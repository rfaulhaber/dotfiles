{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.nixos-generators.nixosModules.all-formats
  ];

  services.openssh.enable = true;

  services.sshd.enable = true;

  users.extraUsers.nixos.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBvwfQy4U/GVd5S2JhNnaQvuKizbavuUWihmr/89fjZo ryan@hyperion"
  ];
}
