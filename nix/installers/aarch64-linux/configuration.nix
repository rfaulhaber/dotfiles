{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  services.openssh.enable = true;

  services.sshd.enable = true;

  users.extraUsers.nixos.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA3oh1zeSI2tjCGtIFIj5H4qvf4tPph3O3mguYVcVCC9 ryan@hyperion"
  ];
}
