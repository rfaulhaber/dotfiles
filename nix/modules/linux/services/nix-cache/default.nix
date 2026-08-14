# Private Nix Binary Cache
#
# Serves the local Nix store as a binary cache for other machines.
# Supports two backends: harmonia (modern, recommended) or nix-serve (simpler).
#
# SETUP:
#
# 1. Generate signing keypair:
#
#    nix key generate-secret --key-name atlas.lan-1 > /path/to/cache-secret-key
#    nix key convert-secret-to-public < /path/to/cache-secret-key > /path/to/cache-public-key
#
# 2. Add secret key to sops (recommended):
#
#    # In secrets.yaml
#    nix-cache-key: <contents of cache-secret-key>
#
#    # In host configuration
#    sops.secrets.nix-cache-key = {};
#
# 3. Enable this module:
#
#    modules.services.nix-cache = {
#      enable = true;
#      backend = "harmonia";
#      port = 5000;
#      secretKeyFile = config.sops.secrets.nix-cache-key.path;
#    };
#
# 4. Configure client machines to use the cache:
#
#    nix.settings = {
#      substituters = [ "http://atlas.lan:5000" ];
#      trusted-public-keys = [ "atlas.lan-1:<PUBLIC_KEY_HERE>" ];
#    };
#
# 5. To link this module, add to nix/modules/linux/services/default.nix:
#
#    imports = [ ... ./nix-cache ... ];
#
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.nix-cache;
in {
  options.modules.services.nix-cache = {
    enable = mkEnableOption "private Nix binary cache";

    backend = mkOption {
      description = "Which binary cache server to use.";
      type = types.enum ["nix-serve" "harmonia"];
      default = "harmonia";
      example = "nix-serve";
    };

    port = mkOption {
      description = "Port to serve the binary cache on.";
      type = types.port;
      default = 8751;
    };

    secretKeyFile = mkOption {
      description = ''
        Path to the secret signing key file. Generate with:
        nix key generate-secret --key-name cache.example.com-1 > /path/to/secret-key
      '';
      type = types.path;
      example = "/run/secrets/nix-cache-secret-key";
    };

    openFirewall = mkOption {
      description = "Whether to open the cache port in the firewall.";
      type = types.bool;
      default = true;
    };

    interface = mkOption {
      description = "Network interface to open the cache port on.";
      type = types.str;
      example = "enp3s0";
    };

    priority = mkOption {
      description = "Priority for this cache (lower = higher priority).";
      type = types.int;
      default = 30;
    };

    # Harmonia-specific options
    harmonia = {
      workers = mkOption {
        description = "Number of worker threads for Harmonia.";
        type = types.nullOr types.int;
        default = null;
      };

      maxConnectionRate = mkOption {
        description = "Maximum number of connections per second.";
        type = types.nullOr types.int;
        default = null;
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.secretKeyFile != null;
        message = "modules.services.nix-cache.secretKeyFile must be set.";
      }
    ];

    # Harmonia backend
    services.harmonia.cache = mkIf (cfg.backend == "harmonia") {
      enable = true;
      signKeyPaths = [cfg.secretKeyFile];
      settings =
        {
          bind = "[::]:${toString cfg.port}";
          inherit (cfg) priority;
        }
        // optionalAttrs (cfg.harmonia.workers != null) {
          workers = cfg.harmonia.workers;
        }
        // optionalAttrs (cfg.harmonia.maxConnectionRate != null) {
          max_connection_rate = cfg.harmonia.maxConnectionRate;
        };
    };

    # nix-serve backend
    services.nix-serve = mkIf (cfg.backend == "nix-serve") {
      enable = true;
      inherit (cfg) port;
      inherit (cfg) secretKeyFile;
      openFirewall = false; # We handle this ourselves for consistency
    };

    # Firewall configuration
    networking.firewall.interfaces.${cfg.interface} = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };

    # Ensure nix-serve package is available for nix-serve backend
    environment.systemPackages = mkIf (cfg.backend == "nix-serve") [
      pkgs.nix-serve
    ];
  };
}
