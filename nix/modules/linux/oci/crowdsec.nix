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

  centralized = cfg.allowlist.centralized;
  centralizedConfigured = centralized.cidrs != [] || centralized.sopsKey != null;

  # The acquis.yaml mounted on top of the persisted /etc/crowdsec directory.
  # Multiple acquisitions are concatenated with --- document separators, which
  # CrowdSec parses as a multi-document YAML stream.
  acquisDocs =
    mapAttrsToList (name: a: {
      source = "file";
      filenames = ["/var/log/${name}/${baseNameOf a.hostPath}"];
      labels = {inherit (a) type;};
    })
    enabledAcquisitions;

  acquisYaml = pkgs.writeText "acquis.yaml" (
    concatStringsSep "\n---\n" (map (d: builtins.toJSON d) acquisDocs)
  );

  # Acquisition mounts: bind the log's containing directory, not the file —
  # a file bind pins the inode, so a producer recreating its log (rotation,
  # container recreate) would silently blind detection. Each source gets its
  # own dir inside the container so the tail buffer is per-source; read-only
  # so we can never overwrite the producer's log. Keep hostPath in a
  # dedicated log directory: every sibling file is visible to the container.
  acquisMounts =
    mapAttrsToList
    (name: a: "${dirOf a.hostPath}:/var/log/${name}:ro")
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
        "scenarios/crowdsecurity/http-crawl-non_statics".

        Only *standalone* hub items can be removed. Scenarios that live as
        sub-documents inside another item's multi-document file (e.g. the
        LePresidente/http-generic-*-bf pair inside
        crowdsecurity/http-generic-bf) have no hub identity of their own and
        the removal fails on every boot — use `simulatedScenarios` for those.

        Use this to suppress overly-aggressive community scenarios that
        transitively get pulled in by base collections. The removal runs in
        ExecStartPost on every service start because the image's entrypoint
        reinstalls collections (and their dependencies) on each container
        boot, so disables don't survive a restart on their own.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["scenarios/crowdsecurity/http-crawl-non_statics"];
    };

    simulatedScenarios = mkOption {
      description = ''
        Scenario names forced into simulation mode: overflows still create
        alerts (visible in cscli and the console) but their decisions are
        marked simulated, which the LAPI strips from every bouncer query —
        the ban is never enforced. Matching is exact string equality against
        the scenario document's `name`, so this reaches sub-scenarios
        bundled inside a multi-document hub file, which `disabledHubItems`
        cannot remove. A typo silently no-ops.

        Renders /etc/crowdsec/simulation.yaml wholesale on every service
        start — manual `cscli simulation` state on the host does not
        survive a restart.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["LePresidente/http-generic-403-bf"];
    };

    acquisitions = mkOption {
      description = ''
        Log sources to ingest. Each entry mounts the directory containing
        `hostPath` into the container at `/var/log/<name>/` and registers
        `/var/log/<name>/<basename>` in acquis.yaml with the given parser
        type. hostPath should live in a dedicated log directory — all of its
        siblings become readable by the CrowdSec container.
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
        Bouncers registered with the LAPI. The container reads
        `BOUNCER_KEY_<name>=<value>` from its env file; an ExecStartPost hook
        re-registers each key on every service start so the LAPI's copy always
        matches the rendered secret (the image entrypoint only adds *absent*
        bouncers, and its registration DB persists across container
        recreates). The key value comes from a sops secret addressable as
        `config.sops.secrets.<sopsKey>`.

        Rotating a key: change the sops value, deploy, then restart this
        service and every consumer of the same secret (e.g. podman-traefik) —
        activation re-renders the secret files but running containers keep the
        old env/mounts until recreated.
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
          which arrive after the parser step — use `centralized` for IPs
          that must never be banned from any source.
        '';
        type = types.listOf types.str;
        default = [];
        example = ["10.0.0.0/8" "203.0.113.42/32"];
      };

      centralized = {
        cidrs = mkOption {
          description = ''
            IPs/CIDRs registered in a LAPI-level *centralized* allowlist
            (`cscli allowlists`, CrowdSec >= 1.6.6) named `nix-managed`.
            Unlike the parser-level `cidrs` above, this vetoes decisions
            from every source: local scenario overflows are dropped before
            the profile step, community-blocklist (CAPI) pulls are filtered
            on import, and adding an entry retroactively expires existing
            decisions against it.

            The list is reconciled (rebuilt from scratch) on every service
            start, which also re-lifts any ban an entry may have picked up —
            so only list addresses that should be unconditionally trusted
            forever. Values here land in the world-readable nix store; for
            identifying addresses (e.g. a home WAN IP) use `sopsKey`.
          '';
          type = types.listOf types.str;
          default = [];
          example = ["203.0.113.42/32"];
        };
        sopsKey = mkOption {
          description = ''
            Sops secret key holding newline-separated IPs/CIDRs (comments
            with `#` allowed) merged into the same `nix-managed` allowlist
            as `cidrs`. Use for addresses that identify people or places.
            The rendered file is mounted into the container and read there,
            so values never appear on a host command line or in the journal.

            Changing the secret's *value* re-renders the file but does not
            restart the container — run `systemctl restart podman-crowdsec`
            after deploying a value change.
          '';
          type = types.nullOr types.str;
          default = null;
          example = "crowdsec/allowlist-cidrs";
        };
      };
      expressions = mkOption {
        description = ''
          expr-lang expressions evaluated at the s02-enrich parser stage.
          Any event for which an expression returns true is whitelisted and
          never reaches a scenario bucket — use this to exclude known-benign
          traffic that would otherwise feed brute-force/scanner scenarios.

          Fields are those set by upstream parsers; guard HTTP-only fields
          with the log_type so non-HTTP events don't error, e.g.
          `evt.Meta.log_type in ['http_access-log', 'http_error-log'] && evt.Meta.http_path startsWith '/foo/'`.

          Unlike cidrs, expressions match regardless of source IP, so they
          survive a client's IP changing.
        '';
        type = types.listOf types.str;
        default = [];
        example = ["evt.Meta.http_path startsWith '/api/v1/auth/newt/'"];
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
      centralizedSecret = optionalAttrs (centralized.sopsKey != null) {
        ${centralized.sopsKey} = {};
      };
    in
      bouncerSecrets // enrollSecret // centralizedSecret;

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
        ++ optional (centralized.sopsKey != null)
        "${config.sops.secrets.${centralized.sopsKey}.path}:/etc/crowdsec/nix-allowlist-cidrs:ro"
        ++ acquisMounts;
      extraOptions =
        ["--network-alias=crowdsec"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "crowdsec";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-crowdsec" = let
      allowlistYaml =
        if cfg.allowlist.cidrs == [] && cfg.allowlist.expressions == []
        then null
        else
          pkgs.writeText "local-allowlist.yaml" (builtins.toJSON {
            name = "local/allowlist";
            description = "Trusted IPs/CIDRs and benign expressions that bypass local CrowdSec scenarios.";
            whitelist =
              {reason = cfg.allowlist.reason;}
              // optionalAttrs (cfg.allowlist.cidrs != []) {
                cidr = cfg.allowlist.cidrs;
              }
              // optionalAttrs (cfg.allowlist.expressions != []) {
                expression = cfg.allowlist.expressions;
              };
          });
      # Rendered even when the list is empty so dropping a scenario from
      # simulatedScenarios re-arms it — a stale exclusions entry would
      # otherwise keep suppressing enforcement invisibly. YAML is a JSON
      # superset, matching the allowlist render above.
      simulationYaml = pkgs.writeText "simulation.yaml" (builtins.toJSON {
        simulation = false;
        exclusions = cfg.simulatedScenarios;
      });
    in
      mkMerge [
        (ociLib.mkServiceConfig {
          inherit (cfg) networks;
          sopsTemplates = ["crowdsec-env"];
          # Map container-level `dependsOn` into systemd ordering: podman's own
          # --depends is service-of-record only and doesn't gate ExecStartPre,
          # so without this the file bind-mounts could race their producers.
          extraAfter = map (c: "podman-${c}.service") cfg.dependsOn;
          extraRequires = map (c: "podman-${c}.service") cfg.dependsOn;
        })
        {
          # Hash the rendered allowlist into restartTriggers so editing the
          # cidrs list re-deploys the container. Same for disabledHubItems
          # and the centralized entries — editing either list should
          # re-trigger the post-start hooks that apply them.
          restartTriggers =
            optional (allowlistYaml != null) (toString allowlistYaml)
            ++ [(toString cfg.disabledHubItems) (toString centralized.cidrs)];
          serviceConfig.ExecStartPre = [
            "${pkgs.writeShellScript "crowdsec-dir-init" ''
              set -euo pipefail
              mkdir -p ${cfg.baseDir}/config ${cfg.baseDir}/data
              # Pre-seeding before first container start is safe: the image
              # entrypoint copies its staging config with --ignore-existing,
              # so this file always wins.
              install -m 0644 ${simulationYaml} ${cfg.baseDir}/config/simulation.yaml
              ${
                optionalString (allowlistYaml != null) ''
                  mkdir -p ${cfg.baseDir}/config/parsers/s02-enrich
                  install -m 0644 ${allowlistYaml} ${cfg.baseDir}/config/parsers/s02-enrich/local-allowlist.yaml
                ''
              }
              ${
                optionalString (cfg.allowlist.cidrs == [] && cfg.allowlist.expressions == []) ''
                  # No allowlist configured — clean up any previous render so
                  # the parser doesn't keep applying a stale list.
                  rm -f ${cfg.baseDir}/config/parsers/s02-enrich/local-allowlist.yaml
                ''
              }
            ''}"
          ];
          serviceConfig.ExecStartPost =
            optional (enabledBouncers != {})
            "+${pkgs.writeShellScript "crowdsec-sync-bouncer-keys" ''
              set -uo pipefail
              # The image entrypoint only registers a bouncer when it's absent,
              # and the registration DB under /var/lib/crowdsec persists across
              # container recreates — so a rotated BOUNCER_KEY_* value would
              # never reach the LAPI, and the bouncer would start 403ing every
              # request once its decision cache expired. Delete + re-add on
              # each start so the LAPI key always matches the rendered env.
              # Same readiness poll/budget as the hub-items hook below.
              for i in $(seq 1 60); do
                if ${pkgs.podman}/bin/podman exec crowdsec cscli bouncers list >/dev/null 2>&1; then
                  break
                fi
                sleep 1
              done

              ${concatMapStringsSep "\n" (name: ''
                # The key is referenced by env-var name and expanded inside the
                # container, so its value never appears on a host command line
                # or in the journal.
                if ! ${pkgs.podman}/bin/podman exec crowdsec sh -c \
                  'cscli bouncers delete ${name} >/dev/null 2>&1; cscli bouncers add ${name} -k "$BOUNCER_KEY_${name}" >/dev/null'; then
                  echo "failed to re-register bouncer ${name}; LAPI may hold a stale key" >&2
                fi
              '') (attrNames enabledBouncers)}
            ''}"
            ++ optional (cfg.disabledHubItems != [])
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
                  if ! ${pkgs.podman}/bin/podman exec crowdsec \
                    cscli ${itemType} remove "${itemName}" --force; then
                    # A name cscli can't resolve is usually a sub-scenario
                    # bundled inside another hub file — removal can never
                    # work for those; the scenario stays ACTIVE.
                    echo "could not remove hub item ${item}; it is still active — verify the name against 'cscli ${itemType} list'" >&2
                  fi
                '')
                cfg.disabledHubItems}

              # Tell the daemon to re-scan its config so the removed items
              # stop being active without a container restart.
              ${pkgs.podman}/bin/podman exec crowdsec kill -HUP 1 || true
            ''}"
            ++ [
              "+${pkgs.writeShellScript "crowdsec-sync-centralized-allowlist" ''
                set -uo pipefail
                # Same readiness poll/budget as the hooks above.
                for i in $(seq 1 60); do
                  if ${pkgs.podman}/bin/podman exec crowdsec cscli allowlists list >/dev/null 2>&1; then
                    break
                  fi
                  sleep 1
                done

                # Rebuild the list from scratch so entries dropped from the
                # nix config disappear from the LAPI too; other allowlists
                # (console-managed) are untouched. `add` retroactively
                # expires any decision recorded against an entry, so the
                # brief delete→add gap self-heals.
                ${pkgs.podman}/bin/podman exec crowdsec \
                  cscli allowlists delete nix-managed >/dev/null 2>&1 || true

                ${optionalString centralizedConfigured ''
                  if ! ${pkgs.podman}/bin/podman exec crowdsec \
                    cscli allowlists create nix-managed -d "nix-managed never-ban allowlist"; then
                    echo "failed to create allowlist nix-managed; its IPs are unprotected until the next service start" >&2
                  fi
                  ${optionalString (centralized.cidrs != []) ''
                    if ! ${pkgs.podman}/bin/podman exec crowdsec \
                      cscli allowlists add nix-managed ${escapeShellArgs centralized.cidrs}; then
                      echo "failed to register static centralized-allowlist entries" >&2
                    fi
                  ''}
                  ${optionalString (centralized.sopsKey != null) ''
                    # The file holds identifying addresses; strip comments and
                    # blanks and feed cscli inside the container so values
                    # never appear on a host command line or in the journal.
                    if ! ${pkgs.podman}/bin/podman exec crowdsec sh -c \
                      'sed -e "s/#.*$//" -e "/^[[:space:]]*$/d" /etc/crowdsec/nix-allowlist-cidrs | xargs -r cscli allowlists add nix-managed'; then
                      echo "failed to register centralized-allowlist entries from the sops file" >&2
                    fi
                  ''}
                ''}
              ''}"
            ];
        }
      ];
  };
}
