{
  config,
  lib,
  pkgs,
  ...
}: {
  users.users.ryan = {
    name = "ryan";
    description = "ryan";
    # TODO do better
    extraGroups = ["wheel" "audio" "lp" "plugdev"];
    isNormalUser = true;
    home = "/home/ryan";
    group = "users";
    uid = 1000;

    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID9EVRAxaCrK68NSCoiNjjQLqu4k13Z45tCBb0jGAtC/ ryan@eos"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPhN+t0aI3pQhZsFPoRn8dWe7YvDn3ehWOUmwvqbQyZP ryan@1p"
    ];
  };
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = lib.mkDefault "no";
    };
    extraConfig = ''
      PermitEmptyPasswords no
      AllowTcpForwarding yes
    '';
    ports = [4697];
  };

  security.pam = {
    sshAgentAuth.enable = true;
    services.ryan.sshAgentAuth = true;
  };
}
