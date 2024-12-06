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
