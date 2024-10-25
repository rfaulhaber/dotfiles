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
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAZQ6dhGnjyJ+SBMeN5IRHcpV6ERR+a/WPmvD7o2TM90 ryan@hyperion"
  ];
}
