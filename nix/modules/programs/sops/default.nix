{
  config,
  lib,
  pkgs,
  inputs,
  hostDir,
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
      type = types.nullOr types.str;
      default = "/etc/sops/age/host.age";
    };
    sshKeyPaths = mkOption {
      description = "Path to default ssh key file.";
      type = types.listOf types.str;
      default = ["/etc/ssh/ssh_host_ed25519_key"];
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.pathExists (hostDir + "/secrets.yaml");
        message = "$host/secrets.yaml must exist";
      }
    ];

    sops = {
      inherit (cfg) secrets;
      # Path arithmetic, NOT "${hostDir}/secrets.yaml": interpolating the
      # directory path copies the entire host dir into the store, so any edit
      # to any file under nix/hosts/<host>/ would rehash the sops manifest and
      # with it the host's toplevel. The concatenated path copies only the one
      # file.
      defaultSopsFile = hostDir + "/secrets.yaml";
      age =
        {
          inherit (cfg) sshKeyPaths;
        }
        // lib.optionalAttrs (cfg.keyFile != null) {
          inherit (cfg) keyFile;
        };
    };

    user.packages = with pkgs; [
      sops
      rage
    ];
  };
}
