{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  services = {
    openssh.enable = true;
    sshd.enable = true;
  };

  users.extraUsers.nixos = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID9EVRAxaCrK68NSCoiNjjQLqu4k13Z45tCBb0jGAtC/ ryan@eos"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPhN+t0aI3pQhZsFPoRn8dWe7YvDn3ehWOUmwvqbQyZP ryan@1p"
    ];

    shell = pkgs.nushell;

    packages = with pkgs; [
      neovim
    ];
  };

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
