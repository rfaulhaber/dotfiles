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
        # nix-direnv's _nix_import_env restores TMPDIR & friends after
        # importing the `nix print-dev-env` dump, but not SHELL, so the dev
        # shell's store bash would leak into every $SHELL consumer (tmux
        # default-shell, `exec $env.SHELL`, ...). Wrap its use_flake — direnv
        # sources lib/*.sh (nix-direnv) before this file — and keep the
        # interactive SHELL.
        eval "$(declare -f use_flake | sed '1s/use_flake/_nix_direnv_use_flake/')"
        use_flake() {
          local saved_shell=''${SHELL:-__UNSET__}
          _nix_direnv_use_flake "$@"
          local status=$?
          if [[ $saved_shell == __UNSET__ ]]; then
            unset SHELL
          else
            export SHELL=$saved_shell
          fi
          return $status
        }
      '';
    };
  };
}
