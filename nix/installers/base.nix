{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  services.openssh.enable = true;

  services.sshd.enable = true;

  users.extraUsers.nixos = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID9EVRAxaCrK68NSCoiNjjQLqu4k13Z45tCBb0jGAtC/ ryan@eos"
    ];

    shell = pkgs.nushell;
  };
}
