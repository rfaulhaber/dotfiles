{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.modules.services.keystats;
  keystats = pkgs.writeShellScriptBin "keystats" ''
    exec ${pkgs.python3}/bin/python3 ${./keystats.py} "$@"
  '';
in {
  options.modules.services.keystats = {
    enable = lib.mkEnableOption "aggregate keyboard statistics collector";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [keystats];

    systemd.services.keystats = {
      description = "Aggregate keyboard statistics collector";
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        ExecStart = "${pkgs.python3}/bin/python3 ${./keystats.py} collect";
        User = config.user.name;
        SupplementaryGroups = ["input"];
        StateDirectory = "keystats";
        StateDirectoryMode = "0750";
        Restart = "on-failure";
        RestartSec = 5;
        # The unit handles keystroke-derived data, so it is denied any
        # network access outright rather than merely sandboxed.
        PrivateNetwork = true;
        IPAddressDeny = "any";
        RestrictAddressFamilies = ["AF_UNIX"];
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictRealtime = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        CapabilityBoundingSet = "";
        UMask = "0077";
      };
    };
  };
}
