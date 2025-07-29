{
  config,
  lib,
  pkgs,
  isLinux,
  ...
}:
with lib; let
  cfg = config.modules.services.ssh;
in {
  imports =
    [
      ./client.nix
    ]
    # I don't have an sshd config for macOS at the moment
    ++ lib.optionals isLinux [
      ./server.nix
    ];

  options.modules.services.ssh = {enable = mkEnableOption false;};
}
