{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.ebay-mcp;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  imageRef = imageLib.renderImage cfg.image;
  registryHost = head (splitString "/" cfg.image.repository);
  # A localhost-published forgejo web port speaks plain HTTP, so pulls
  # through it need the pull-scoped flag rather than a host-wide
  # insecure-registry entry. Pulls via the public TLS name keep full
  # verification.
  insecurePull = hasPrefix "localhost:" registryHost;

  updateRun = pkgs.writeShellApplication {
    name = "ebay-mcp-update";
    runtimeInputs = [pkgs.podman pkgs.systemd];
    text = ''
      new="$(podman pull --quiet ${optionalString insecurePull "--tls-verify=false "}${imageRef})"
      current="$(podman container inspect --format '{{.Image}}' ebay-mcp 2>/dev/null || echo none)"
      if [ "$new" != "$current" ]; then
        systemctl restart podman-ebay-mcp.service
      fi
    '';
  };
in {
  options.modules.linux.oci.services.ebay-mcp = {
    enable = mkEnableOption "eBay Browse MCP server (Streamable HTTP behind its own OAuth provider)";

    # The private image on the forgejo registry, under its canonical public
    # name. On the host that runs forgejo itself, override the repository
    # to the localhost-published web port so the pull depends on nothing
    # off-host (no Pangolin hairpin). The digest-refresh tooling only
    # speaks anonymous TLS registries, so oci-images.json does not manage
    # this image; CI pushes land on :latest and the update timer below
    # picks them up.
    image = imageLib.mkImageOptions {
      repository = "git.3679.space/private/ebay-mcp-server";
      version = "latest";
    };

    publicUrl = mkOption {
      description = ''
        The MCP endpoint URL exactly as entered into the client, e.g.
        https://mcp.example.com/mcp. Clients byte-compare it during OAuth
        discovery, and its path component is the route the server mounts
        MCP on — the OAuth endpoints and /.well-known documents live on
        the same origin, so the reverse proxy must forward the whole
        vhost.
      '';
      type = types.str;
      example = "https://mcp.example.com/mcp";
    };

    accountDeletionEndpoint = mkOption {
      description = ''
        Mount eBay's marketplace account-deletion compliance webhook at
        /ebay/account-deletion (eBay requires the subscription for every
        production keyset) and declare the ebay-mcp/verification-token
        secret that answers eBay's challenge.
      '';
      type = types.bool;
      default = true;
    };

    update = {
      enable = mkOption {
        description = "Re-pull the tag on a timer and restart the container when the registry has a newer image.";
        type = types.bool;
        default = true;
      };

      onCalendar = mkOption {
        description = "Re-pull cadence.";
        type = types.str;
        default = "daily";
      };

      randomizedDelaySec = mkOption {
        description = "Jitter applied to each timer firing.";
        type = types.str;
        default = "15m";
      };
    };

    networks = mkOption {
      description = "Networks to join. The tunnel's newt reaches the server by alias, so the shared default bridge suffices.";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      description = "Extra environment variables (e.g. EBAY_MARKETPLACE_ID, EBAY_BUYER_COUNTRY, EBAY_MCP_LOG).";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion =
            config.modules.linux.oci.registryAuth.enable
            && hasAttr registryHost config.modules.linux.oci.registryAuth.registries;
          message = "ebay-mcp pulls from the private registry ${registryHost}; give it a modules.linux.oci.registryAuth.registries entry.";
        }
      ];

      modules.linux.oci.networks = listToAttrs (
        map (n: nameValuePair n {enable = true;}) cfg.networks
      );

      # The keyset pair is identifying (it names the eBay developer
      # account) on top of the secret Cert ID; access-key is what a human
      # types on the OAuth consent page; token-key is the HMAC key sealing
      # every issued token, so rotating it revokes them all at once.
      sops.secrets =
        {
          "ebay-mcp/client-id" = {};
          "ebay-mcp/client-secret" = {};
          "ebay-mcp/access-key" = {};
          "ebay-mcp/token-key" = {};
        }
        // optionalAttrs cfg.accountDeletionEndpoint {
          "ebay-mcp/verification-token" = {};
        };

      sops.templates."ebay-mcp-env".content =
        ''
          EBAY_CLIENT_ID=${config.sops.placeholder."ebay-mcp/client-id"}
          EBAY_CLIENT_SECRET=${config.sops.placeholder."ebay-mcp/client-secret"}
          EBAY_MCP_ACCESS_KEY=${config.sops.placeholder."ebay-mcp/access-key"}
          EBAY_MCP_TOKEN_KEY=${config.sops.placeholder."ebay-mcp/token-key"}
        ''
        + optionalString cfg.accountDeletionEndpoint ''
          EBAY_MCP_VERIFICATION_TOKEN=${config.sops.placeholder."ebay-mcp/verification-token"}
        '';

      # No volumes and no host port: the server is stateless (OAuth
      # artifacts are self-contained HMAC blobs, so restarts don't
      # disconnect clients), and it's only reachable through the tunnel —
      # newt resolves the network alias, and the image's baked
      # EBAY_MCP_HTTP_ADDR=0.0.0.0:8080 is where it listens.
      virtualisation.oci-containers.containers.ebay-mcp = {
        image = imageRef;
        inherit (cfg) dependsOn;
        environment = {EBAY_MCP_PUBLIC_URL = cfg.publicUrl;} // cfg.extraEnv;
        environmentFiles = [config.sops.templates."ebay-mcp-env".path];
        extraOptions =
          ["--network-alias=ebay-mcp"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
          # The container pulls inside podman run, so the pull-scoped
          # plain-HTTP flag rides along here.
          ++ optional insecurePull "--tls-verify=false"
          ++ imageLib.mkImageLabels {
            module = "ebay-mcp";
            inherit (cfg) image;
          };
        log-driver = "journald";
      };

      systemd.services."podman-ebay-mcp" = mkMerge [
        (ociLib.mkServiceConfig {
          inherit (cfg) networks;
          sopsTemplates = ["ebay-mcp-env"];
          # When the registry-hosting forgejo lives on this same host,
          # don't race its container at boot; elsewhere Restart=always
          # retries cover a failed pull.
          extraAfter =
            ["network-online.target"]
            ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
        })
        {
          wants =
            ["network-online.target"]
            ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
        }
      ];
    }

    (mkIf cfg.update.enable {
      systemd.services.ebay-mcp-update = {
        description = "ebay-mcp image update";
        # Not an oci-containers unit, so registry-auth.nix's blanket env
        # injection misses it — wire the authfile in directly.
        environment.REGISTRY_AUTH_FILE = config.modules.linux.oci.registryAuth.authFile;
        after =
          ["network-online.target"]
          ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
        wants =
          ["network-online.target"]
          ++ optional config.modules.linux.oci.services.forgejo.enable "podman-forgejo.service";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = getExe updateRun;
        };
      };

      systemd.timers.ebay-mcp-update = {
        wantedBy = ["timers.target"];
        timerConfig = {
          OnCalendar = cfg.update.onCalendar;
          RandomizedDelaySec = cfg.update.randomizedDelaySec;
          Persistent = true;
        };
      };
    })
  ]);
}
