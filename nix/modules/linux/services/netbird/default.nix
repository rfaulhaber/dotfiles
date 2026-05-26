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
        #
        # ManagementURL/AdminURL are typed `*url.URL` in netbird's Config; Go's
        # encoding/json serializes them as nested objects and cannot unmarshal
        # a plain string here. Match the on-disk shape netbird itself writes.
        # The management API is exposed on :33073 by the OCI module
        # (nix/modules/linux/oci/netbird.nix); the dashboard is on :443.
        config = {
          ManagementURL = {
            Scheme = "https";
            Host = "netbird.3679.space:33073";
          };
          AdminURL = {
            Scheme = "https";
            Host = "netbird.3679.space:443";
          };
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
