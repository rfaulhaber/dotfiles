{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.crowdsec;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  enabledBouncers = filterAttrs (_: b: b.enable) cfg.bouncers;
  enabledAcquisitions = filterAttrs (_: a: a.enable) cfg.acquisitions;

  # The acquis.yaml mounted on top of the persisted /etc/crowdsec directory.
  # Multiple acquisitions are concatenated with --- document separators, which
  # CrowdSec parses as a multi-document YAML stream.
  acquisDocs =
    mapAttrsToList (name: a: {
      source = "file";
      filenames = ["/var/log/${name}/${baseNameOf a.hostPath}"];
      labels = {type = a.type;};
    })
    enabledAcquisitions;

  acquisYaml = pkgs.writeText "acquis.yaml" (
    concatStringsSep "\n---\n" (map (d: builtins.toJSON d) acquisDocs)
  );

  # Acquisition mounts: each log file lives in its own dir inside the
  # container so CrowdSec's tail buffer is per-source. Read-only to make
  # sure we never overwrite the producer's log.
  acquisMounts =
    mapAttrsToList
    (name: a: "${a.hostPath}:/var/log/${name}/${baseNameOf a.hostPath}:ro")
    enabledAcquisitions;
in {
  options.modules.linux.oci.services.crowdsec = {
    enable = mkEnableOption "CrowdSec engine (log-based intrusion detection with bouncer integration)";

    image = imageLib.mkImageOptions {
      repository = "crowdsecurity/crowdsec";
      version = "latest";
    };

    baseDir = mkOption {
      description = "Root directory for CrowdSec state (config + decision DB).";
      type = types.str;
      example = "/docker/config/crowdsec";
    };

    networks = mkOption {
      description = ''
        Podman networks the engine joins. Must include any network on which a
        bouncer needs to reach the LAPI by `crowdsec:8080` alias.
      '';
      type = types.listOf types.str;
      default = ["default"];
    };

    timezone = mkOption {
      type = types.str;
      default = "America/New_York";
    };

    collections = mkOption {
      description = ''
        CrowdSec hub collections to install at boot. Defaults give Linux baseline
        plus Traefik HTTP brute-force + scanner/CVE detection. The image's
        entrypoint installs these idempotently; adding/removing a collection
        takes effect on next container start.
      '';
      type = types.listOf types.str;
      default = [
        "crowdsecurity/linux"
        "crowdsecurity/traefik"
        "crowdsecurity/base-http-scenarios"
        "crowdsecurity/http-cve"
      ];
    };

    disabledHubItems = mkOption {
      description = ''
        Hub items (scenarios, parsers, collections, postoverflows) to remove
        after CrowdSec installs collections at startup. Each entry has the
        form "<type>/<author>/<name>" — e.g.
        "scenarios/LePresidente/http-generic-403-bf".

        Use this to suppress overly-aggressive community scenarios that
        transitively get pulled in by base collections. The removal runs in
        ExecStartPost on every service start because the image's entrypoint
        reinstalls collections (and their dependencies) on each container
        boot, so disables don't survive a restart on their own.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["scenarios/LePresidente/http-generic-403-bf"];
    };

    acquisitions = mkOption {
      description = ''
        Log sources to ingest. Each entry mounts `hostPath` into the container at
        `/var/log/<name>/<basename>` and registers it in acquis.yaml with the
        given parser type.
      '';
      default = {};
      type = types.attrsOf (types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            default = true;
          };
          type = mkOption {
            description = ''
              CrowdSec parser type label (e.g., "traefik", "syslog"). Must match a
              parser provided by one of the installed collections.
            '';
            type = types.str;
            example = "traefik";
          };
          hostPath = mkOption {
            description = "Absolute host path of the log file to tail.";
            type = types.str;
            example = "/docker/config/traefik/access.log";
          };
        };
      });
    };

    bouncers = mkOption {
      description = ''
        Bouncers to pre-register on first boot. The container reads
        `BOUNCER_KEY_<name>=<value>` from its env file and runs
        `cscli bouncers add` if absent. The key value comes from a sops secret
        addressable as `config.sops.secrets.<sopsKey>`.
      '';
      default = {};
      type = types.attrsOf (types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            default = true;
          };
          sopsKey = mkOption {
            description = "Sops secret key holding this bouncer's API key.";
            type = types.str;
            example = "crowdsec/bouncer-api-key";
          };
        };
      });
    };

    enroll = {
      enable = mkEnableOption "CrowdSec console enrollment (dashboard visibility at app.crowdsec.net)";
      sopsKey = mkOption {
        description = "Sops secret key holding the enrollment key from app.crowdsec.net.";
        type = types.str;
        default = "crowdsec/enrollment-key";
      };
      instanceName = mkOption {
        description = "Human-readable instance name shown in the CrowdSec console.";
        type = types.nullOr types.str;
        default = null;
        example = "janus";
      };
    };

    communityBlocklist = {
      enable = mkOption {
        description = ''
          Subscribe to CrowdSec's Central API (CAPI) — pulls the community
          blocklist for enforcement, and shares local detections back. For a
          self-hosted instance without high-volume attack traffic, the
          downside (community-list IPs banning legitimate users e.g. on
          mobile carrier ranges or residential ISPs once flagged) often
          outweighs the upside. Disabling keeps local detection fully
          functional and does not affect console enrollment.
        '';
        type = types.bool;
        default = true;
      };
    };

    allowlist = {
      cidrs = mkOption {
        description = ''
          IP addresses and CIDR ranges that should never be flagged by
          CrowdSec's *local* scenarios. Renders a parser-level whitelist
          at /etc/crowdsec/parsers/s02-enrich/local-allowlist.yaml.

          Note: this does NOT filter community-blocklist (CAPI) decisions
          which arrive after the parser step. If your concern is CAPI
          false positives on user IPs you can't enumerate in advance, set
          communityBlocklist.enable = false instead.
        '';
        type = types.listOf types.str;
        default = [];
        example = ["10.0.0.0/8" "203.0.113.42/32"];
      };
      reason = mkOption {
        description = "Annotation attached to whitelisted events for debugging.";
        type = types.str;
        default = "trusted by nix-managed allowlist";
      };
    };

    dependsOn = mkOption {
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    # Declare every referenced sops secret. Bouncer keys are deduped because
    # the same secret can back multiple bouncer entries in theory.
    sops.secrets = let
      bouncerSecrets = listToAttrs (
        map (b: nameValuePair b.sopsKey {}) (attrValues enabledBouncers)
      );
      enrollSecret = optionalAttrs cfg.enroll.enable {
        ${cfg.enroll.sopsKey} = {};
      };
    in
      bouncerSecrets // enrollSecret;

    # Env file consumed by the container. Holds bouncer keys (sensitive) and
    # the enrollment key (sensitive). Non-secret env (COLLECTIONS, TZ, instance
    # name) lives in the inline environment block below.
    sops.templates."crowdsec-env" = {
      content = let
        bouncerLines =
          mapAttrsToList
          (name: b: "BOUNCER_KEY_${name}=${config.sops.placeholder.${b.sopsKey}}")
          enabledBouncers;
        enrollLines = optionals cfg.enroll.enable [
          "ENROLL_KEY=${config.sops.placeholder.${cfg.enroll.sopsKey}}"
        ];
      in
        concatStringsSep "\n" (bouncerLines ++ enrollLines) + "\n";
    };

    virtualisation.oci-containers.containers.crowdsec = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          TZ = cfg.timezone;
          COLLECTIONS = concatStringsSep " " cfg.collections;
          GID = "0";
        }
        // optionalAttrs (cfg.enroll.enable && cfg.enroll.instanceName != null) {
          ENROLL_INSTANCE_NAME = cfg.enroll.instanceName;
        }
        // optionalAttrs (!cfg.communityBlocklist.enable) {
          DISABLE_ONLINE_API = "true";
        }
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."crowdsec-env".path];
      volumes =
        [
          "${cfg.baseDir}/config:/etc/crowdsec:rw"
          "${cfg.baseDir}/data:/var/lib/crowdsec/data:rw"
          "${acquisYaml}:/etc/crowdsec/acquis.yaml:ro"
        ]
        ++ acquisMounts;
      extraOptions =
        ["--network-alias=crowdsec"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "crowdsec";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-crowdsec" = let
      allowlistYaml =
        if cfg.allowlist.cidrs == []
        then null
        else
          pkgs.writeText "local-allowlist.yaml" (builtins.toJSON {
            name = "local/allowlist";
            description = "Trusted IPs/CIDRs that bypass local CrowdSec scenarios.";
            whitelist = {
              reason = cfg.allowlist.reason;
              cidr = cfg.allowlist.cidrs;
            };
          });
    in
      mkMerge [
        (ociLib.mkServiceConfig {
          networks = cfg.networks;
          sopsTemplates = ["crowdsec-env"];
          # Map container-level `dependsOn` into systemd ordering: podman's own
          # --depends is service-of-record only and doesn't gate ExecStartPre,
          # so without this the file bind-mounts could race their producers.
          extraAfter = map (c: "podman-${c}.service") cfg.dependsOn;
          extraRequires = map (c: "podman-${c}.service") cfg.dependsOn;
        })
        {
          # Hash the rendered allowlist into restartTriggers so editing the
          # cidrs list re-deploys the container. Same for disabledHubItems —
          # editing the list should re-trigger the post-start removal.
          restartTriggers =
            optional (allowlistYaml != null) (toString allowlistYaml)
            ++ [(toString cfg.disabledHubItems)];
          serviceConfig.ExecStartPre = [
            "${pkgs.writeShellScript "crowdsec-dir-init" ''
              set -euo pipefail
              mkdir -p ${cfg.baseDir}/config ${cfg.baseDir}/data
              ${
                optionalString (allowlistYaml != null) ''
                  mkdir -p ${cfg.baseDir}/config/parsers/s02-enrich
                  install -m 0644 ${allowlistYaml} ${cfg.baseDir}/config/parsers/s02-enrich/local-allowlist.yaml
                ''
              }
              ${
                optionalString (cfg.allowlist.cidrs == []) ''
                  # No allowlist configured — clean up any previous render so
                  # the parser doesn't keep applying a stale list.
                  rm -f ${cfg.baseDir}/config/parsers/s02-enrich/local-allowlist.yaml
                ''
              }
            ''}"
          ];
          serviceConfig.ExecStartPost =
            optional (cfg.disabledHubItems != [])
            "+${pkgs.writeShellScript "crowdsec-disable-hub-items" ''
              set -uo pipefail
              # The container has just been created by ExecStart; the daemon
              # inside may still be loading hub items. Poll cscli until it
              # answers, then issue the removes. 60s budget is comfortable
              # for cold image pulls; subsequent restarts converge in <5s.
              for i in $(seq 1 60); do
                if ${pkgs.podman}/bin/podman exec crowdsec cscli decisions list >/dev/null 2>&1; then
                  break
                fi
                sleep 1
              done

              ${concatMapStringsSep "\n" (item: let
                  parts = lib.splitString "/" item;
                  itemType = lib.head parts;
                  itemName = lib.concatStringsSep "/" (lib.tail parts);
                in ''
                  # Remove ${item}. --force suppresses "in use by collection"
                  # warnings; the collection install itself is left untouched.
                  ${pkgs.podman}/bin/podman exec crowdsec \
                    cscli ${itemType} remove "${itemName}" --force || true
                '')
                cfg.disabledHubItems}

              # Tell the daemon to re-scan its config so the removed items
              # stop being active without a container restart.
              ${pkgs.podman}/bin/podman exec crowdsec kill -HUP 1 || true
            ''}";
        }
      ];
  };
}
