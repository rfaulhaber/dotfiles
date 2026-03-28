{
  config,
  lib,
  pkgs,
  inputs,
  isLinux,
  ...
}:
with lib; let
  cfg = config.modules.programs.firefox;

  extensionPackages = with pkgs.firefox-addons;
    [
      ublock-origin
      multi-account-containers
      onepassword-password-manager
    ]
    ++ cfg.extraExtensions;

  # Intentional settings distilled from the live profile.
  # These are the deliberate choices — ephemeral/session state is excluded.
  profileSettings = {
    # -- Privacy & Security (strict ETP) --
    "browser.contentblocking.category" = "strict";
    "privacy.donottrackheader.enabled" = true;
    "privacy.fingerprintingProtection" = true;
    "privacy.trackingprotection.enabled" = true;
    "privacy.trackingprotection.socialtracking.enabled" = true;
    "privacy.trackingprotection.emailtracking.enabled" = true;
    "privacy.query_stripping.enabled" = true;
    "privacy.query_stripping.enabled.pbmode" = true;
    "privacy.bounceTrackingProtection.mode" = 1;
    "privacy.annotate_channels.strict_list.enabled" = true;

    # -- Network privacy --
    "network.dns.disablePrefetch" = true;
    "network.prefetch-next" = false;
    "network.http.speculative-parallel-limit" = 0;
    "network.http.referer.disallowCrossSiteRelaxingDefault.top_navigation" = true;

    # -- Disable built-in AI/ML features --
    "browser.ml.enable" = false;
    "browser.ml.chat.enabled" = false;
    "browser.ml.chat.menu" = false;

    # -- New tab page: no sponsored content --
    "browser.newtabpage.activity-stream.showSponsored" = false;
    "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
    "browser.newtabpage.activity-stream.feeds.section.topstories" = false;

    # -- UI preferences --
    "sidebar.revamp" = true;
    "sidebar.verticalTabs" = true;
    "browser.tabs.inTitlebar" = 0;
    "browser.fullscreen.autohide" = false;
    "findbar.highlightAll" = true;

    # -- Dark theme --
    "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
    "browser.theme.content-theme" = 0;
    "browser.theme.toolbar-theme" = 0;

    # -- Containers --
    "privacy.userContext.enabled" = true;
    "privacy.userContext.ui.enabled" = true;

    # -- DRM (for streaming services) --
    "media.eme.enabled" = true;

    # -- Disable password manager (using 1Password) --
    "signon.rememberSignons" = false;

    # -- Disable autofill (using 1Password) --
    "extensions.formautofill.addresses.enabled" = false;
    "extensions.formautofill.creditCards.enabled" = false;

    # -- Search --
    "browser.search.visualSearch.featureGate" = false;

    # -- Sanitize on shutdown --
    "privacy.clearOnShutdown_v2.formdata" = true;
    "privacy.clearHistory.formdata" = true;
  };
in {
  options.modules.programs.firefox = {
    enable = mkEnableOption "Firefox browser";

    package = mkOption {
      type = types.package;
      description = "Firefox package to use.";
      default = pkgs.firefox-devedition;
    };

    profilePath = mkOption {
      type = types.str;
      description = "Relative path under ~/.mozilla/firefox/ for the managed profile. Set to the existing directory name to preserve your data.";
      default = "default";
    };

    setDefaultPDFViewer = mkOption {
      type = types.bool;
      description = "Set Firefox as the default PDF viewer.";
      default = false;
    };

    search = {
      default = mkOption {
        type = types.str;
        description = "Default search engine ID for the URL bar.";
        default = "ddg";
      };

      private = mkOption {
        type = types.str;
        description = "Default search engine ID in private browsing.";
        default = "ddg";
      };
    };

    sync = mkOption {
      type = types.bool;
      description = "Whether Firefox Sync is expected (affects UI hints).";
      default = true;
    };

    extraExtensions = mkOption {
      type = types.listOf types.package;
      description = "Additional extension packages beyond the base set.";
      default = [];
    };

    extraSettings = mkOption {
      type = types.attrsOf (types.oneOf [types.bool types.int types.str]);
      description = "Additional about:config preferences merged into the profile.";
      default = {};
    };

    containers = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          id = mkOption {
            type = types.int;
            description = "Container userContextId.";
          };
          icon = mkOption {
            type = types.str;
            default = "circle";
          };
          color = mkOption {
            type = types.str;
            default = "blue";
          };
        };
      });
      description = "Firefox Multi-Account Containers.";
      default = {
        Personal = {
          id = 1;
          icon = "fingerprint";
          color = "blue";
        };
        Private = {
          id = 6;
          icon = "chill";
          color = "pink";
        };
        Facebook = {
          id = 8;
          icon = "fence";
          color = "toolbar";
        };
      };
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [inputs.firefox-addons.overlays.default];

    home.programs.firefox = {
      enable = true;
      package = cfg.package;
      # Version=2 makes Firefox key profiles by install-path hash (installs.ini).
      # On NixOS the store path changes every rebuild, producing a new hash each
      # time. Firefox then tries to register the "new install" by writing to
      # profiles.ini, which is a read-only symlink into the nix store → crash.
      # Version=1 skips installs.ini entirely and just uses profiles.ini directly.
      profileVersion = 1;

      # Enterprise policies — applied system-wide, no profile dependency
      policies = {
        DisableTelemetry = true;
        DisableFirefoxStudies = true;
        DisablePocket = true;
        DontCheckDefaultBrowser = true;
        SearchBar = "unified";
        EnableTrackingProtection = {
          Value = true;
          Locked = false;
          Cryptomining = true;
          Fingerprinting = true;
          EmailTracking = true;
        };
        FirefoxHome = {
          Search = true;
          TopSites = true;
          SponsoredTopSites = false;
          Highlights = false;
          Pocket = false;
          SponsoredPocket = false;
        };
        UserMessaging = {
          WhatsNew = false;
          ExtensionRecommendations = false;
          FeatureRecommendations = false;
          UrlbarInterventions = false;
          SkipOnboarding = true;
          MoreFromMozilla = false;
          FirefoxLabs = false;
        };
      };

      profiles.default = {
        isDefault = true;
        path = cfg.profilePath;
        extensions.packages = extensionPackages;

        containers =
          mapAttrs (name: c: {
            inherit (c) id icon color;
          })
          cfg.containers;

        settings = profileSettings // cfg.extraSettings;

        search = {
          default = cfg.search.default;
          privateDefault = cfg.search.private;
          force = true;
          engines = {
            "Nix Packages" = {
              urls = [{template = "https://search.nixos.org/packages?type=packages&query={searchTerms}";}];
              icon = "''${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = ["@np"];
            };
            "NixOS Options" = {
              urls = [{template = "https://search.nixos.org/options?query={searchTerms}";}];
              icon = "''${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = ["@no"];
            };
            "NixOS Wiki" = {
              urls = [{template = "https://wiki.nixos.org/w/index.php?search={searchTerms}";}];
              definedAliases = ["@nw"];
            };
          };
        };
      };
    };

    # Firefox unconditionally rewrites profiles.ini on every startup.
    # home-manager symlinks it into the nix store (read-only), so Firefox
    # crashes with "profile cannot be loaded." Replace the symlink with a
    # mutable copy after home-manager link generation.
    home-manager.users.${config.user.name}.home.activation.firefoxProfilesWritable = inputs.home-manager.lib.hm.dag.entryAfter ["linkGeneration"] ''
      profilesIni="$HOME/.mozilla/firefox/profiles.ini"
      if [ -L "$profilesIni" ]; then
        target=$(readlink "$profilesIni")
        rm "$profilesIni"
        cp "$target" "$profilesIni"
        chmod u+w "$profilesIni"
      fi
    '';

    # PDF viewer default via XDG mime
    environment.etc."xdg/mimeapps.list" = mkIf (cfg.setDefaultPDFViewer && isLinux) {
      text = ''
        [Default Applications]
        application/pdf=${cfg.package.pname}.desktop
      '';
    };
  };
}
