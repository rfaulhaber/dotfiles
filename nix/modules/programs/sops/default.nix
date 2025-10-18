{
  config,
  lib,
  pkgs,
  inputs,
  hostDir,
  hostname,
  isLinux,
  isDarwin,
  ...
}:
with lib; let
  cfg = config.modules.programs.sops;
in {
  imports =
    lib.optionals isLinux [
      inputs.sops-nix.nixosModules.sops
    ]
    ++ lib.optionals isDarwin [
      inputs.sops-nix.darwinModules.sops
    ];
  options.modules.programs.sops = {
    enable = mkEnableOption false;
    secrets = mkOption {
      description = "Secrets associated with this host.";
      type = types.attrs;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.pathExists "${hostDir}/secrets.yaml";
        message = "$host/secrets.yaml must exist";
      }
    ];

    sops = {
      defaultSopsFile = "${hostDir}/secrets.yaml";
      age = {
        sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
        keyFile = "${config.user.home}/.config/sops/age/keys.txt";
      };
      secrets."unsplash" = {
        owner = config.user.name;
        group = config.user.group;
        mode = "0440";
      };
    };
  };
}
