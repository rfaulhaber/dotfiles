{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.direnv;
in {
  options.modules.programs.direnv = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    home.programs.direnv = {
      enable = true;
      enableNushellIntegration = mkIf config.modules.programs.nushell.enable true;
      nix-direnv.enable = true;
      stdlib = ''
        use_flake() {
          watch_file flake.nix
          watch_file flake.lock
          eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
        }
      '';
    };
  };
}
