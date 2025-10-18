{
  config,
  lib,
  pkgs,
  inputs,
  hostDir,
  ...
}:
with lib; let
  cfg = config.modules.programs.age;
in {
  imports = [inputs.ragenix.nixosModules.default];
  options.modules.programs.age = {
    enable = mkEnableOption false;
    agenix = {
      enable = mkOption {
        description = "If enabled, uses agenix.";
        type = types.bool;
        default = false;
      };
      secretsDir = mkOption {
        description = "Secrets dir for this config.";
        type = types.either types.str types.path;
        default = "${hostDir}/secrets";
      };
    };
  };

  config = mkIf cfg.enable {
    # TODO make configurable
    age = mkIf cfg.agenix.enable {
      identityPaths = [
        "${config.user.home}/.ssh/id_host"
        # NB: make sure this actually exists!!!
        # I would try and assert it, but...
        # NB again: this is necessary because /home is mounted after agenix
        # starts
        "/etc/ssh/ssh_host_ed25519_key"
      ];

      secrets =
        "${cfg.secretsDir}/secrets.nix"
        |> import
        |> lib.attrNames
        |> (map (name: let
          nameWithoutSuffix = lib.removeSuffix ".age" name;
        in {
          "${nameWithoutSuffix}" = {
            file = "${cfg.secretsDir}/${name}";
            owner = config.user.name;
          };
        }))
        |> (foldl (x: y: x // y) {});
    };

    user.packages = with pkgs;
      [
        rage
      ]
      ++ lib.optionals cfg.agenix.enable [
        inputs.ragenix.packages.${pkgs.system}.default
      ];
  };
}
