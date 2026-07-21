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
        _restore_or_unset() {
          local key=$1 value=$2
          if [[ $value == __UNSET__ ]]; then
            unset "$key"
          else
            export "$key=$value"
          fi
        }

        # `nix print-dev-env` dumps the build sandbox's SHELL (a store bash)
        # and a transient TMPDIR into the environment; importing those turns
        # every $SHELL consumer (tmux default-shell, `exec $env.SHELL`, ...)
        # into bash and points TMPDIR at a directory nix may delete. Keep the
        # interactive values instead, mirroring nix-direnv's _nix_import_env.
        use_flake() {
          watch_file flake.nix
          watch_file flake.lock
          local -A saved=(
            [SHELL]=''${SHELL:-__UNSET__}
            [NIX_BUILD_TOP]=''${NIX_BUILD_TOP:-__UNSET__}
            [TMP]=''${TMP:-__UNSET__}
            [TMPDIR]=''${TMPDIR:-__UNSET__}
            [TEMP]=''${TEMP:-__UNSET__}
            [TEMPDIR]=''${TEMPDIR:-__UNSET__}
          )
          eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
          local key
          for key in "''${!saved[@]}"; do
            _restore_or_unset "$key" "''${saved[$key]}"
          done
        }
      '';
    };
  };
}
