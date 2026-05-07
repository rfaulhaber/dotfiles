{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.soularr;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  # Render a single section of the config.ini. Values are emitted
  # verbatim — soularr's configparser doesn't do any quoting, so callers
  # supply already-stringified scalar values.
  renderSection = name: kvs: let
    pairs = mapAttrsToList (k: v:
      if v == null
      then null
      else "${k} = ${v}")
    kvs;
    nonNull = filter (l: l != null) pairs;
  in "[${name}]\n${concatStringsSep "\n" nonNull}\n";

  boolStr = b:
    if b
    then "True"
    else "False";

  # Build each [section] from its option subtree. Secrets are inlined as
  # sops placeholders — sops-nix substitutes them when the template is
  # rendered at activation time.
  configIniContent = let
    lidarr = renderSection "Lidarr" {
      api_key = config.sops.placeholder."soularr/lidarr-api-key";
      host_url = cfg.lidarr.hostUrl;
      download_dir = cfg.lidarr.downloadDir;
      disable_sync = boolStr cfg.lidarr.disableSync;
    };
    slskd = renderSection "Slskd" {
      api_key = config.sops.placeholder."soularr/slskd-api-key";
      host_url = cfg.slskd.hostUrl;
      url_base = cfg.slskd.urlBase;
      download_dir = cfg.slskd.downloadDir;
      delete_searches = boolStr cfg.slskd.deleteSearches;
      stalled_timeout = toString cfg.slskd.stalledTimeout;
    };
    release = renderSection "Release Settings" {
      use_most_common_tracknum = boolStr cfg.releaseSettings.useMostCommonTracknum;
      allow_multi_disc = boolStr cfg.releaseSettings.allowMultiDisc;
      accepted_countries = concatStringsSep "," cfg.releaseSettings.acceptedCountries;
      accepted_formats = concatStringsSep "," cfg.releaseSettings.acceptedFormats;
    };
    search = renderSection "Search Settings" {
      search_timeout = toString cfg.searchSettings.searchTimeout;
      maximum_peer_queue = toString cfg.searchSettings.maximumPeerQueue;
      minimum_peer_upload_speed = toString cfg.searchSettings.minimumPeerUploadSpeed;
      minimum_filename_match_ratio = toString cfg.searchSettings.minimumFilenameMatchRatio;
      allowed_filetypes = concatStringsSep "," cfg.searchSettings.allowedFiletypes;
      ignored_users = concatStringsSep "," cfg.searchSettings.ignoredUsers;
      search_for_tracks = boolStr cfg.searchSettings.searchForTracks;
      album_prepend_artist = boolStr cfg.searchSettings.albumPrependArtist;
      track_prepend_artist = boolStr cfg.searchSettings.trackPrependArtist;
      search_type = cfg.searchSettings.searchType;
      number_of_albums_to_grab = toString cfg.searchSettings.numberOfAlbumsToGrab;
      remove_wanted_on_failure = boolStr cfg.searchSettings.removeWantedOnFailure;
      title_blacklist = concatStringsSep "," cfg.searchSettings.titleBlacklist;
      search_source = cfg.searchSettings.searchSource;
    };
    logging = renderSection "Logging" {
      level = cfg.logging.level;
      format = cfg.logging.format;
      datefmt = cfg.logging.datefmt;
    };
  in
    concatStringsSep "\n" [lidarr slskd release search logging];
in {
  options.modules.linux.oci.services.soularr = {
    enable = mkEnableOption "Soularr (lidarr → slskd bridge)";

    image = imageLib.mkImageOptions {
      repository = "mrusse08/soularr";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for soularr state (mounted at /data inside the
        container — soularr expects its config.ini there, not at /config).
        config.ini itself is bind-mounted on top from the sops template,
        so the on-disk copy is shadowed.
      '';
      type = types.str;
      example = "/data/apps/soularr";
    };

    slskdDownloadsDir = mkOption {
      description = ''
        Host directory containing slskd's completed downloads (mounted at
        /downloads). Soularr scans this to match completed Soulseek
        downloads to lidarr requests. Should match slskd.downloadsDir.
      '';
      type = types.str;
      example = "/data/slskd";
    };

    scriptInterval = mkOption {
      description = "Seconds between soularr scheduler runs.";
      type = types.int;
      default = 300;
    };

    user = {
      uid = mkOption {
        description = "UID to run the container as (soularr uses --user, not PUID).";
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
        Required for soularr in this setup: it reaches slskd and lidarr
        at localhost:<port>, and both of those services live inside the
        gluetun netns.
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
      default = ["slskd" "lidarr"];
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };

    # ----- config.ini fields -------------------------------------------

    lidarr = {
      hostUrl = mkOption {
        description = "URL Lidarr uses (e.g., what soularr connects to).";
        type = types.str;
        default = "http://gluetun:8686";
      };
      downloadDir = mkOption {
        description = "Path to slskd downloads inside the Lidarr container.";
        type = types.str;
        default = "/downloads/slskd";
      };
      disableSync = mkOption {
        description = "If true, Lidarr won't auto-import from Slskd.";
        type = types.bool;
        default = false;
      };
    };

    slskd = {
      hostUrl = mkOption {
        description = "URL slskd uses.";
        type = types.str;
        default = "http://gluetun:5030";
      };
      urlBase = mkOption {
        description = "URL base path for slskd.";
        type = types.str;
        default = "/";
      };
      downloadDir = mkOption {
        description = "Download path inside the slskd container.";
        type = types.str;
        default = "/downloads";
      };
      deleteSearches = mkOption {
        description = "Delete search after Soularr runs.";
        type = types.bool;
        default = false;
      };
      stalledTimeout = mkOption {
        description = "Max seconds to wait for downloads (prevents infinite hangs).";
        type = types.int;
        default = 3600;
      };
    };

    releaseSettings = {
      useMostCommonTracknum = mkOption {
        description = "Pick release with most common track count.";
        type = types.bool;
        default = true;
      };
      allowMultiDisc = mkOption {
        description = "Allow multi-disc releases.";
        type = types.bool;
        default = true;
      };
      acceptedCountries = mkOption {
        description = "Accepted release countries.";
        type = types.listOf types.str;
        default = ["Europe" "Japan" "United Kingdom" "United States" "[Worldwide]" "Australia" "Canada"];
      };
      acceptedFormats = mkOption {
        description = "Accepted release formats.";
        type = types.listOf types.str;
        default = ["CD" "Digital Media" "Vinyl"];
      };
    };

    searchSettings = {
      searchTimeout = mkOption {
        description = "Search timeout in milliseconds.";
        type = types.int;
        default = 5000;
      };
      maximumPeerQueue = mkOption {
        description = "Maximum peer queue size.";
        type = types.int;
        default = 50;
      };
      minimumPeerUploadSpeed = mkOption {
        description = "Minimum upload speed (bits/sec).";
        type = types.int;
        default = 0;
      };
      minimumFilenameMatchRatio = mkOption {
        description = "Minimum match ratio between Lidarr track and Soulseek filename (0-1).";
        type = types.float;
        default = 0.8;
      };
      allowedFiletypes = mkOption {
        description = ''
          Preferred file types and qualities (most to least preferred).
          Use "flac" or "mp3" alone to ignore quality details.
        '';
        type = types.listOf types.str;
        default = ["flac 24/192" "flac 16/44.1" "flac" "mp3 320" "mp3"];
      };
      ignoredUsers = mkOption {
        description = "Soulseek users to ignore.";
        type = types.listOf types.str;
        default = [];
      };
      searchForTracks = mkOption {
        description = ''
          Set to false to only search for album titles. Soularr does not
          search for individual tracks regardless — this setting controls
          whether track titles factor into the search at all.
        '';
        type = types.bool;
        default = true;
      };
      albumPrependArtist = mkOption {
        description = "Prepend artist name when searching for albums.";
        type = types.bool;
        default = false;
      };
      trackPrependArtist = mkOption {
        description = "Prepend artist name when searching for tracks.";
        type = types.bool;
        default = true;
      };
      searchType = mkOption {
        description = ''
          Search mode: "all" searches every wanted record, "first_page"
          repeatedly searches the first page, "incrementing_page" starts
          with the first page and increments on each run.
        '';
        type = types.enum ["all" "incrementing_page" "first_page"];
        default = "incrementing_page";
      };
      numberOfAlbumsToGrab = mkOption {
        description = "Albums to process per run.";
        type = types.int;
        default = 10;
      };
      removeWantedOnFailure = mkOption {
        description = "Unmonitor album on failure (logs to failure_list.txt).";
        type = types.bool;
        default = false;
      };
      titleBlacklist = mkOption {
        description = "Blacklist words in album or track titles (case-insensitive).";
        type = types.listOf types.str;
        default = [];
      };
      searchSource = mkOption {
        description = "Lidarr search source.";
        type = types.enum ["missing" "cutoff_unmet"];
        default = "missing";
      };
    };

    logging = {
      level = mkOption {
        description = "Python logging level.";
        type = types.enum ["DEBUG" "INFO" "WARNING" "ERROR" "CRITICAL"];
        default = "INFO";
      };
      format = mkOption {
        description = "Python logging format string.";
        type = types.str;
        default = "[%(levelname)s|%(module)s|L%(lineno)d] %(asctime)s: %(message)s";
      };
      datefmt = mkOption {
        description = "Python logging date format.";
        type = types.str;
        default = "%Y-%m-%dT%H:%M:%S%z";
      };
    };
  };

  config = mkIf cfg.enable (let
    netOpts =
      (
        if cfg.useGluetun
        then ["--network=container:${cfg.gluetunContainer}"]
        else
          ["--network-alias=soularr"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
      )
      ++ imageLib.mkImageLabels {
        module = "soularr";
        image = cfg.image;
      };
    gluetunDeps = optional cfg.useGluetun "podman-${cfg.gluetunContainer}.service";
  in {
    sops.secrets = {
      "soularr/lidarr-api-key" = {};
      "soularr/slskd-api-key" = {};
    };

    sops.templates."soularr-config-ini" = {
      content = configIniContent;
      # World-readable through the bind mount: container runs as
      # cfg.user.uid (default 1000) and would otherwise hit EACCES.
      mode = "0444";
    };

    virtualisation.oci-containers.containers.soularr = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = {
        "TZ" = cfg.timezone;
        "SCRIPT_INTERVAL" = toString cfg.scriptInterval;
      };
      volumes = [
        "${cfg.slskdDownloadsDir}:/downloads"
        "${cfg.baseDir}:/data"
        "${config.sops.templates."soularr-config-ini".path}:/data/config.ini:ro"
      ];
      extraOptions =
        netOpts
        ++ ["--user=${toString cfg.user.uid}:${toString cfg.user.gid}"];
      log-driver = "journald";
    };

    systemd.services."podman-soularr" = ociLib.mkServiceConfig {
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
