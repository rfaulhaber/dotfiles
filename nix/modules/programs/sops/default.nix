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
    userKeyFile = mkOption {
      description = ''
        age identity file the `sops` CLI reads when the primary user edits
        secrets by hand, as opposed to `keyFile`, which sops-nix uses at
        activation. Exported as `SOPS_AGE_KEY_FILE` into the user's shell;
        `null` leaves sops to its platform default.
      '';
      type = types.nullOr types.str;
      default = "${config.user.home}/.config/sops/age/keys.txt";
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

    # sops looks for the user identity in ~/.config/sops/age/keys.txt on Linux
    # but ~/Library/Application Support/sops/age/keys.txt on macOS. The file
    # lives at the ~/.config path on every host, so pin it instead of leaning
    # on the Linux default. Set through home-manager directly: `home.programs`
    # is a shallow-merged attrs alias (see options.nix), so a key added under
    # `home.programs.nushell` here would clobber the nushell module's definition.
    home-manager.users.${config.user.name}.programs.nushell.environmentVariables = mkIf (cfg.userKeyFile != null) {
      SOPS_AGE_KEY_FILE = cfg.userKeyFile;
    };

    user.packages = with pkgs; [
      sops
      rage
    ];
  };
}
