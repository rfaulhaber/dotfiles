{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.project-notes;

  projectNotes = lib.my.writeNushellScriptBin pkgs "project-notes" (
    ''
      $env.PROJECT_NOTES_ROOT = ($env.PROJECT_NOTES_ROOT? | default "${cfg.root}")
    ''
    + builtins.readFile ../../../../bin/project-notes.nu
  );
in {
  options.modules.programs.project-notes = {
    enable = mkEnableOption "the project-notes command";

    root = mkOption {
      type = types.str;
      default = "~/.project-notes";
      description = ''
        Directory, synced between machines (Syncthing, say), holding one notes
        directory per repository. `project-notes` replaces the current
        checkout's untracked `notes` directory with a symlink to
        `<root>/<repo>`, moving any existing notes across. Must be absolute or
        start with `~/`; a Nix path literal would copy the directory into the
        store.
      '';
    };
  };

  config = mkIf cfg.enable {
    user.packages = [projectNotes];
  };
}
