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
      description = "Secrets associated with this host. Passthrough attributes to `sops.secrets.<secret>...`";
      type = types.attrs;
      default = {};
    };
    keyFile = mkOption {
      description = "Path to default system key file.";
      type = types.str;
      default = "/etc/sops/age/host.age";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.pathExists "${hostDir}/secrets.yaml";
        message = "$host/secrets.yaml must exist";
      }
    ];

    sops = {
      inherit (cfg) secrets;
      defaultSopsFile = "${hostDir}/secrets.yaml";
      age = {
        inherit (cfg) keyFile;
        sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
      };
    };

    user.packages = with pkgs; [
      sops
      rage
    ];
  };
}
