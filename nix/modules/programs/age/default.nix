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
  agenix = inputs.agenix;
in {
  imports = [inputs.agenix.nixosModules.default];
  options.modules.programs.age = {
    enable = mkEnableOption false;
    secretsDir = mkOption {
      description = "Secrets dir for this config.";
      type = types.either types.str types.path;
      default = "${hostDir}/secrets";
    };
  };

  config = mkIf cfg.enable {
    age = {
      identityPaths = [
        "${config.user.home}/.ssh/id_host"
        # NB: make sure this actually exists!!!
        # I would try and assert it, but...
        # NB again: this is necessary because /home is mounted after agenix
        # starts
        "/etc/ssh/ssh_host_ed25519_key"
      ];

      secrets = lib.pipe "${cfg.secretsDir}/secrets.nix" [
        import
        lib.attrNames
        (map (name: let
          nameWithoutSuffix = lib.removeSuffix ".age" name;
        in {
          "${nameWithoutSuffix}" = {
            file = "${cfg.secretsDir}/${name}";
            owner = config.user.name;
          };
        }))
        (foldl (x: y: x // y) {})
      ];
    };

    user.packages = with pkgs; [
      age
      # rage
      agenix.packages.${pkgs.system}.default
    ];
  };
}
