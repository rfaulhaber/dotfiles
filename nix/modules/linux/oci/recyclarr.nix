{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.recyclarr;
  ociLib = config.modules.linux.oci.lib;

  instanceOpts = {name, ...}: {
    options = {
      kind = mkOption {
        description = "Recyclarr instance kind.";
        type = types.enum ["sonarr" "radarr"];
      };

      baseUrl = mkOption {
        description = "Base URL recyclarr uses to reach this *arr instance.";
        type = types.str;
        example = "http://gluetun:8989";
      };

      apiKeySecret = mkOption {
        description = ''
          Sops secret name holding this instance's API key. The module
          declares `sops.secrets.<name>` automatically and references the
          value in secrets.yml as `!secret <instance-name>_api_key`.
        '';
        type = types.str;
        example = "recyclarr/sonarr-main-api-key";
      };

      body = mkOption {
        description = ''
          Raw YAML body for this instance — everything that goes under
          `<kind>.<name>:` besides `base_url` and `api_key`. Module renders
          it inline below the auto-generated metadata, indented to match
          the surrounding instance block.
        '';
        type = types.lines;
        default = "";
        example = ''
          quality_definition:
            type: series
          include:
            - template: sonarr-quality-definition-series
        '';
      };
    };
  };

  enabledInstances = {
    # TODO generate yaml more dynamically
    sonarr_main = {
      kind = "sonarr";
      baseUrl = "http://gluetun:8989";
      apiKeySecret = "recyclarr/sonarr-main-api-key";
      body = ''
        quality_definition:
          type: series
      '';
    };
    radarr_main = {
      kind = "radarr";
      baseUrl = "http://gluetun:7878";
      apiKeySecret = "recyclarr/radarr-main-api-key";
      body = ''
        include:
          # Comment out any of the following includes to disable them
          - template: radarr-quality-definition-movie
          - template: radarr-quality-profile-hd-bluray-web
          - template: radarr-custom-formats-hd-bluray-web
          - template: radarr-quality-definition-anime
          - template: radarr-quality-profile-anime
          - template: radarr-custom-formats-anime
          - template: radarr-quality-definition-movie
          - template: radarr-quality-profile-uhd-bluray-web
          - template: radarr-custom-formats-uhd-bluray-web

        custom_formats:
          - trash_ids:
              - 570bc9ebecd92723d2d21500f4be314c
              - eca37840c13c6ef2dd0262b141a5482f
              - e0c07d59beb37348e975a930d5e50319
              - 9d27d9d2181838f76dee150882bdc58c
              - db9b4c4b53d312a3ca5f1378f6440fc9
              - 957d0f44b592285f26449575e8b1167e
            assign_scores_to:
              - name: HD Bluray + WEB

          - trash_ids:
            assign_scores_to:
              - name: HD Bluray + WEB

          - trash_ids:
            assign_scores_to:
              - name: HD Bluray + WEB
          - trash_ids:
              - 064af5f084a0a24458cc8ecd3220f93f
            assign_scores_to:
              - name: Remux-1080p - Anime
                score: 0 # Adjust scoring as desired

          - trash_ids:
              - a5d148168c4506b55cf53984107c396e
            assign_scores_to:
              - name: Remux-1080p - Anime
                score: 0 # Adjust scoring as desired

          - trash_ids:
              - 4a3b087eea2ce012fcc1ce319259a3be
            assign_scores_to:
              - name: Remux-1080p - Anime
                score: 0 # Adjust scoring as desired

          - trash_ids:
              - 496f355514737f7d83bf7aa4d24f8169
              - 2f22d89048b01681dde8afe203bf2e95
              - 417804f7f2c4308c1f4c5d380d4c4475
              - 1af239278386be2919e1bcee0bde047e
              - 3cafb66171b47f226146a0770576870f
              - dcf3ec6938fa32445f590a4da84256cd
              - a570d4a0e56a2874b64e5bfa55202a1b
              - e7c2fcae07cbada050a0af3357491d7b
              - 8e109e50e0a0b83a5098b056e13bf6db
              - 185f1dd7264c4562b9022d963ac37424
              - f9f847ac70a0af62ea4a08280b859636
              - 1c1a4c5e823891c75bc50380a6866f73
              - 240770601cc226190c367ef59aba7463
              - c2998bd0d90ed5621d8df281e839436e
            assign_scores_to:
              - name: UHD Bluray + WEB

          - trash_ids:
              - 570bc9ebecd92723d2d21500f4be314c
              - eca37840c13c6ef2dd0262b141a5482f
              - e0c07d59beb37348e975a930d5e50319
              - 9d27d9d2181838f76dee150882bdc58c
              - db9b4c4b53d312a3ca5f1378f6440fc9
            assign_scores_to:
              - name: UHD Bluray + WEB

          - trash_ids:
            assign_scores_to:
              - name: UHD Bluray + WEB

          - trash_ids:
            assign_scores_to:
              - name: UHD Bluray + WEB

          - trash_ids:
            assign_scores_to:
              - name: UHD Bluray + WEB

          - trash_ids:
              - 9c38ebb7384dada637be8899efa68e6f
            assign_scores_to:
              - name: UHD Bluray + WEB
      '';
    };
  };

  instancesByKind = kind: filterAttrs (_: i: i.kind == kind) enabledInstances;

  indentBody = body: indent:
    if body == ""
    then ""
    else let
      pad = concatStrings (genList (_: " ") indent);
      lines = splitString "\n" (removeSuffix "\n" body);
      indented = map (l:
        if l == ""
        then ""
        else "${pad}${l}")
      lines;
    in
      concatStringsSep "\n" indented + "\n";

  renderInstance = name: i:
    "  ${name}:\n"
    + "    base_url: ${i.baseUrl}\n"
    + "    api_key: !secret ${name}_api_key\n"
    + (indentBody i.body 4);

  renderKindBlock = kind: let
    instances = instancesByKind kind;
  in
    if instances == {}
    then ""
    else "${kind}:\n" + concatStringsSep "" (mapAttrsToList renderInstance instances);

  recyclarrYamlContent =
    "# Generated by NixOS OCI recyclarr module — do not edit on disk\n"
    + (renderKindBlock "sonarr")
    + (renderKindBlock "radarr")
    + (optionalString (cfg.extraConfig != "") "\n${cfg.extraConfig}\n");

  recyclarrYamlFile =
    pkgs.writeText "recyclarr.yml" recyclarrYamlContent;
in {
  options.modules.linux.oci.services.recyclarr = {
    enable = mkEnableOption "Recyclarr TRaSH-Guides quality profile sync";

    image = mkOption {
      description = "Recyclarr container image.";
      type = types.str;
      default = "ghcr.io/recyclarr/recyclarr:latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for recyclarr state (mounted at /config). Holds the
        cached TRaSH-Guides repo and run logs. recyclarr.yml and secrets.yml
        are bind-mounted on top of this directory from the nix store /
        sops template paths, so on-disk copies are shadowed.
      '';
      type = types.str;
      example = "/data/apps/recyclarr";
    };

    user = {
      uid = mkOption {
        description = "UID to run the container as.";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID to run the container as.";
        type = types.int;
        default = 100;
      };
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    useGluetun = mkOption {
      description = ''
        Route through the gluetun VPN container's network namespace.
        Required when recyclarr's targets (sonarr/radarr) live inside
        gluetun — it reaches them at localhost:8989/7878.
      '';
      type = types.bool;
      default = false;
    };

    gluetunContainer = mkOption {
      description = "Name of the gluetun container to share netns with.";
      type = types.str;
      default = "gluetun";
    };

    networks = mkOption {
      description = "Networks to join (only used when useGluetun = false).";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
      example = ["radarr" "sonarr"];
    };

    sonarrUrl = mkOption {
      description = "Url for sonarr";
      type = types.str;
      default = null;
    };

    radarrUrl = mkOption {
      description = "Url for radarr";
      type = types.str;
      default = null;
    };

    instances = mkOption {
      description = ''
        Recyclarr instances. Each entry becomes a `<kind>.<name>:` block
        in recyclarr.yml with auto-generated `base_url`/`api_key` and the
        `body` content rendered below.
      '';
      type = types.attrsOf (types.submodule instanceOpts);
      default = {};
      example = literalExpression ''
        {
          sonarr_main = {
            kind = "sonarr";
            baseUrl = "http://gluetun:8989";
            apiKeySecret = "recyclarr/sonarr-main-api-key";
            body = '''
              quality_definition:
                type: series
            ''';
          };
        }
      '';
    };

    extraConfig = mkOption {
      description = "Raw YAML appended to the end of recyclarr.yml (global settings, etc.).";
      type = types.lines;
      default = "";
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    netOpts =
      if cfg.useGluetun
      then ["--network=container:${cfg.gluetunContainer}"]
      else
        ["--network-alias=recyclarr"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";

    # secrets.yml is a flat key→value YAML file. Recyclarr resolves
    # `!secret <key>` references in recyclarr.yml against this file.
    secretsYamlContent =
      concatStringsSep "\n" (mapAttrsToList (name: i: "${name}_api_key: ${config.sops.placeholder.${i.apiKeySecret}}")
        enabledInstances)
      + "\n";
  in {
    sops.secrets =
      listToAttrs (mapAttrsToList (_: i: nameValuePair i.apiKeySecret {}) enabledInstances);

    sops.templates."recyclarr-secrets-yml" = {
      content = secretsYamlContent;
      # World-readable through the bind mount: container runs as a
      # non-root UID and would otherwise hit EACCES on /config/secrets.yml.
      # The file still lives under /run/secrets-rendered/ which is itself
      # access-controlled.
      mode = "0444";
    };

    virtualisation.oci-containers.containers.recyclarr = {
      image = cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "TZ" = cfg.timezone;
      };
      volumes = [
        "${cfg.baseDir}:/config"
        "${recyclarrYamlFile}:/config/recyclarr.yml:ro"
        "${config.sops.templates."recyclarr-secrets-yml".path}:/config/secrets.yml:ro"
      ];
      extraOptions =
        netOpts
        ++ [
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
          "--security-opt=no-new-privileges:true"
        ];
      log-driver = "journald";
    };

    systemd.services."podman-recyclarr" = ociLib.mkServiceConfig {
      networks =
        if cfg.useGluetun
        then []
        else cfg.networks;
      extraAfter = gluetunDeps;
      extraRequires = gluetunDeps;
    };

    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = mkIf (!cfg.useGluetun) (
      listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
    );
  });
}
