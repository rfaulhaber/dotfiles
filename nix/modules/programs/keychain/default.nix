{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.keychain;
in {
  options.modules.programs.keychain = {
    enable = mkEnableOption false;
    keys = mkOption {
      description = "Keys to add to keychain.";
      default = ["A2205925F3B6C5B96F26C3CB544650C5A306061B"];
      type = types.listOf types.str;
    };
  };

  config = mkIf cfg.enable {
    home.programs.keychain = {
      enable = true;
      agents = ["ssh" "gpg"];
      keys = cfg.keys;
      enableZshIntegration = config.modules.programs.zsh.setDefault;
    };
  };
}
