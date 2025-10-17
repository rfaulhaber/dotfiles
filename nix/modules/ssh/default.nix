{
  lib,
  isLinux,
  ...
}: {
  imports =
    [
      ./client.nix
    ]
    # I don't have an sshd config for macOS at the moment
    ++ lib.optionals isLinux [
      ./server.nix
    ];

  options.modules.services.ssh.enable = lib.mkEnableOption false;
}
