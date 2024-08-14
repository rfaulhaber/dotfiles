{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.programs.age;
  agenix = inputs.agenix;
in {
  imports = [ inputs.agenix.nixosModules.default ];
  options.modules.programs.age = {
    enable = mkEnableOption false;
    secrets = mkOption {
      description = "Secrets for this config.";
      type = types.attrs;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    age = {
      inherit (cfg) secrets;
    };

    environment.systemPackages = with pkgs; [
      age
      agenix.packages.${pkgs.system}.default
    ];
  };
}
