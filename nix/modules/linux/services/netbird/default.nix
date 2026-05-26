{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.netbird;
in {
  options.modules.services.netbird = {
    enable = mkEnableOption false;
    autoStart = mkOption {
      type = types.bool;
      default = true;
      description = "Whether or not to auto start the default netbird interface.";
    };
    setupKeyFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = literalExpression ''config.sops.secrets."netbird/setup-key".path'';
      description = ''
        Path to a file containing a Netbird Setup Key. When set, the client
        auto-enrolls and connects to the network on first boot via a sibling
        `netbird-login.service` (no manual `netbird up` required).
      '';
    };
  };

  config = mkIf cfg.enable {
    services.netbird = {
      enable = true;
      clients.default = {
        autoStart = cfg.autoStart;
        # Point the client at the self-hosted Netbird control plane. Merged
        # into /var/lib/netbird/config.json at preStart, so the daemon enrolls
        # against the right management server (default would be api.netbird.io).
        config = {
          ManagementURL = "https://netbird.3679.space";
          AdminURL = "https://netbird.3679.space";
        };
        login = mkIf (cfg.setupKeyFile != null) {
          enable = true;
          setupKeyFile = cfg.setupKeyFile;
          systemdDependencies =
            lib.optional config.modules.programs.sops.enable
            "sops-install-secrets.service";
        };
      };
    };

    user.packages = with pkgs;
      [
        netbird
      ]
      ++ lib.optionals config.modules.desktop.enable [
        netbird-ui
      ];
  };
}
