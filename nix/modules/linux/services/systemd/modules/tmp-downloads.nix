{
  config,
  lib,
  ...
}: let
  inherit (lib) mkEnableOption mkOption mkIf types;
  cfg = config.modules.services.systemd.modules.tmp-downloads;
in {
  options.modules.services.systemd.modules.tmp-downloads = {
    enable = mkEnableOption "ephemeral tmpfs Downloads directory";
    targetDir = mkOption {
      description = ''
        Where to place the Downloads symlink. Supports tmpfiles.d
        specifiers such as %h (home) and %u (username).
      '';
      type = types.str;
      default = "%h/Downloads";
    };
  };

  config = mkIf cfg.enable {
    # Point Downloads at a per-user directory in /tmp (tmpfs, wiped on
    # reboot). systemd-tmpfiles recreates the directory each session, so the
    # symlink is never left dangling after a /tmp clean.
    #   d   create the backing directory
    #   L+  create the symlink, force-removing anything already at targetDir
    systemd.user.tmpfiles.rules = [
      "d /tmp/%u-downloads 0755 - - -"
      "L+ ${cfg.targetDir} - - - - /tmp/%u-downloads"
    ];
  };
}
