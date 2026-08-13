{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.recyclarr;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  enabledInstances = {
    # TODO generate yaml more dynamically
    sonarr_main = {
      kind = "sonarr";
      baseUrl = "http://gluetun:8989";
      apiKeySecret = "recyclarr/sonarr-main-api-key";
      # WEB-1080p (Alternative) rather than plain WEB-1080p: the strict profile
      # permits only WEB 1080p and would leave three quarters of this library
      # outside its allowed set, while the alternative adds Bluray and HDTV at
      # 1080p plus the whole 720p tier. Neither admits remux, which is moot here
      # anyway — 21 of 13,000 episode files are remux and none are 2160p.
      #
      # The SD tail (SDTV, DVD, 480p/576p — roughly a quarter of the library) is
      # outside even this profile. Those series are better left on a permissive
      # profile than marked permanently unmet; assigning series is a separate
      # decision from defining the profile.
      #
      # until_score 0, unlike radarr's: the only custom formats here are audio
      # tiebreakers, and re-downloading an episode because a DD+ release turned
      # up is not worth the churn. Scores decide which release wins at grab
      # time and nothing else.
      body = ''
        quality_definition:
          type: series

        quality_profiles:
          - trash_id: 9d142234e45d6143785ac55f5a9e8dc9 # WEB-1080p (Alternative)
            reset_unmatched_scores:
              enabled: true
            upgrade:
              allowed: true
              until_score: 0

        custom_formats:
          - trash_ids:
              - 4232a509ce60c4e208d13825b7c06264 # DD+ ATMOS
            assign_scores_to:
              - name: "WEB-1080p (Alternative)"
                score: 20

          - trash_ids:
              - 63487786a8b01b7f20dd2bc90dd4a477 # DD+
            assign_scores_to:
              - name: "WEB-1080p (Alternative)"
                score: 15

          - trash_ids:
              - dbe00161b08a25ac6154c55f95e6318d # DD
              - a50b8a0c62274a7c38b09a9619ba9d86 # AAC
            assign_scores_to:
              - name: "WEB-1080p (Alternative)"
                score: 10
      '';
    };
    radarr_main = {
      kind = "radarr";
      baseUrl = "http://gluetun:7878";
      apiKeySecret = "recyclarr/radarr-main-api-key";
      # Guide-backed profiles carry only their own tier formats, so the
      # custom_formats below are additive rather than duplicates.
      #
      # Every profile here excludes remux (the guide pair by construction —
      # Remux-2160p and Remux-1080p are allowed=false — and UHD Preferred by
      # not listing remux qualities), which is the property worth preserving:
      # remux is what produces 60GB files, not 2160p.
      #
      # UHD Preferred is not from the guide: the guide ships no 4K profile
      # with an HD fallback (UHD Bluray + WEB is 4K-only, so a movie with no
      # 4K release stays wanted forever). It exists for manual per-movie
      # assignment and is deliberately not the requestrr default. WEB 2160p
      # outranks Bluray-2160p — inverting the guide's UHD cutoff — because
      # the encodes are roughly half the size and carry DD+/Atmos the
      # clients play natively, where Bluray audio forces a transcode. Below
      # 2160p it falls back to exactly the HD Bluray + WEB ladder. SDR (no
      # WEBDL) and DV (w/o HDR fallback) ride at the guide's -10000: with
      # min_format_score 0 the release is rejected at grab time and the grab
      # falls down the ladder — SDR 4K WEB stays eligible (often better than
      # the 1080p), SDR Bluray encodes and fallback-less DV do not.
      #
      # Scores are calibrated against the edition tags, which the guide sets at
      # 25 (125 for Special Edition). Audio sits below that on purpose. The
      # clients in use reject DTS, TrueHD, FLAC and PCM and fall back to a
      # transcode, so codecs that survive playback get a small edge — but this
      # library is curated by edition, and a Criterion pressing should not lose
      # to a generic release that happens to carry DD+.
      #
      # until_score is set because the guide ships cutoffFormatScore 10000,
      # which nothing here can reach — 145 is the ceiling (Special Edition plus
      # the best audio tiebreaker). Left at the guide value every movie stays
      # format-cutoff-unmet forever. Note the key is upgrade.until_score;
      # there is no cutoff_format_score key in recyclarr's schema.
      body = ''
        quality_definition:
          type: movie

        quality_profiles:
          - trash_id: d1d67249d3890e49bc12e275d989a7e9 # HD Bluray + WEB
            reset_unmatched_scores:
              enabled: true
            upgrade:
              allowed: true
              until_score: 145
          - trash_id: 64fb5f9858489bdac2af690e27c8f42f # UHD Bluray + WEB
            reset_unmatched_scores:
              enabled: true
            upgrade:
              allowed: true
              until_score: 145
          # Custom profile; qualities are listed most-preferred first.
          - name: UHD Preferred
            reset_unmatched_scores:
              enabled: true
            upgrade:
              allowed: true
              until_quality: WEB 2160p
              until_score: 145
            qualities:
              - name: WEB 2160p
                qualities:
                  - WEBDL-2160p
                  - WEBRip-2160p
              - name: Bluray-2160p
              - name: Bluray-1080p
              - name: WEB 1080p
                qualities:
                  - WEBDL-1080p
                  - WEBRip-1080p
              - name: Bluray-720p

        custom_formats:
          # Movie Versions
          - trash_ids:
              - 570bc9ebecd92723d2d21500f4be314c # Remaster
              - eca37840c13c6ef2dd0262b141a5482f # 4K Remaster
              - e0c07d59beb37348e975a930d5e50319 # Criterion Collection
              - 9d27d9d2181838f76dee150882bdc58c # Masters of Cinema
              - db9b4c4b53d312a3ca5f1378f6440fc9 # Vinegar Syndrome
              - 957d0f44b592285f26449575e8b1167e # Special Edition
            assign_scores_to:
              - name: HD Bluray + WEB
              - name: UHD Preferred

          # Movie Versions + SDR
          - trash_ids:
              - 570bc9ebecd92723d2d21500f4be314c # Remaster
              - eca37840c13c6ef2dd0262b141a5482f # 4K Remaster
              - e0c07d59beb37348e975a930d5e50319 # Criterion Collection
              - 9d27d9d2181838f76dee150882bdc58c # Masters of Cinema
              - db9b4c4b53d312a3ca5f1378f6440fc9 # Vinegar Syndrome
              - 9c38ebb7384dada637be8899efa68e6f # SDR
            assign_scores_to:
              - name: UHD Bluray + WEB

          # HDR guards for the 4K cascade, both at the guide's -10000.
          - trash_ids:
              - 25c12f78430a3a23413652cbd1d48d77 # SDR (no WEBDL)
              - 923b6abef9b17f937fab56cfcf89e1f1 # DV (w/o HDR fallback)
            assign_scores_to:
              - name: UHD Preferred

          # Audio, scored as a tiebreaker between otherwise equal releases.
          # Only the codecs the clients can play are listed; DTS, TrueHD, FLAC
          # and PCM are left unmanaged rather than penalised, because for
          # catalog titles there is often only one release in existence and
          # transcoding it beats not having it.
          - trash_ids:
              - 1af239278386be2919e1bcee0bde047e # DD+ ATMOS
            assign_scores_to:
              - name: HD Bluray + WEB
                score: 20
              - name: UHD Bluray + WEB
                score: 20
              - name: UHD Preferred
                score: 20

          - trash_ids:
              - 185f1dd7264c4562b9022d963ac37424 # DD+
            assign_scores_to:
              - name: HD Bluray + WEB
                score: 15
              - name: UHD Bluray + WEB
                score: 15
              - name: UHD Preferred
                score: 15

          - trash_ids:
              - c2998bd0d90ed5621d8df281e839436e # DD
              - 240770601cc226190c367ef59aba7463 # AAC
            assign_scores_to:
              - name: HD Bluray + WEB
                score: 10
              - name: UHD Bluray + WEB
                score: 10
              - name: UHD Preferred
                score: 10
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

    # Upstream publishes no `latest` tag — it is explicitly disabled in their
    # release CI, and their docs direct users to a major-version tag so that
    # breaking majors stay opt-in.
    image = imageLib.mkImageOptions {
      repository = "ghcr.io/recyclarr/recyclarr";
      version = "8";
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
      (
        if cfg.useGluetun
        then ["--network=container:${cfg.gluetunContainer}"]
        else
          ["--network-alias=recyclarr"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
      )
      ++ imageLib.mkImageLabels {
        module = "recyclarr";
        image = cfg.image;
      };
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
      image = imageLib.renderImage cfg.image;
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
      sopsTemplates = ["recyclarr-secrets-yml"];
    };

    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = mkIf (!cfg.useGluetun) (
      listToAttrs (map (n: nameValuePair n {enable = true;}) cfg.networks)
    );
  });
}
