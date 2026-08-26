{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    inputs.determinate.nixosModules.default
  ];

  nix.extraOptions = ''
    !include ${config.sops.templates."nix-access-tokens.conf".path}
  '';

  sops.templates."nix-access-tokens.conf" = {
    content = ''
      access-tokens = github.com=${config.sops.placeholder.github}
    '';
    owner = config.user.name;
    group = config.user.group;
    mode = "0440";
  };

  modules = {
    nix = {
      bigHost = true;
      # Deploys to the whole fleet build here.
      substituters.enable = true;
    };
    programs = {
      btop.enable = true;
      emacs = {
        enable = true;
        package = pkgs.emacs-git;
      };
      ghostty = {
        enable = true;
        fontSize = 20;
      };
      _1password = {
        enable = true;
        autostart = true;
      };
      git = {
        enable = true;
        useDelta = true;
      };
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
        # `hints` (command-history ghost text) defaults to the bright-blue ANSI
        # slot; dim it to the comment grey so it reads as a suggestion, not input.
        colorOverrides.hints = config.modules.themes.colors.grey;
        plugins = with pkgs.nushellPlugins; [
          polars
        ];
      };
      direnv.enable = true;
      devenv.enable = true;
      zellij = {
        enable = true;
        web.enable = true;
      };
      sops = {
        enable = true;
        secrets = {
          unsplash = {
            owner = config.user.name;
            group = config.user.group;
            mode = "0440";
          };
          # used by nix.extraOptions to set access-tokens for github.com
          # and avoid rate limits when fetching flake inputs
          github = {};
          "netbird/setup-key" = {};
          # root's dispatch key for remote builds on vulcan
          vulcan-builder-ssh-key = {};
          anthropic-api-key = {
            owner = config.user.name;
            group = config.user.group;
            mode = "0400";
          };
          openrouter-crush-api-key = {
            owner = config.user.name;
            group = config.user.group;
            mode = "0400";
          };
        };
      };
      claude.enable = true;
      crush = {
        enable = true;
        openrouterApiKeySecret = "openrouter-crush-api-key";
        providers = {
          openrouter = {};
          # vulcan (192.168.0.105) serves a native Ollama on the LAN with its
          # firewall open. `discover_models = true` keeps auto-discovery on even
          # though `models` is non-empty, so every model vulcan hosts still shows
          # up in the picker (merged via /v1/models + /api/show enrichment).
          # Discovery auto-triggers only when `models` is empty, so listing Gemma
          # below would otherwise silently disable it. User-listed models win over
          # discovered ones by ID, so the Gemma entry keeps its settings.
          vulcan = {
            name = "vulcan (ollama)";
            type = "ollama";
            base_url = "http://192.168.0.105:11434/v1/";
            discover_models = true;
            models = [
              {
                name = "Gemma 4 26b";
                id = "gemma4:26b";
                context_window = 32768;
                default_max_tokens = 8192;
                supports_tools = true;
                # Gemma 4 reasons by default; at vulcan's ~16 tok/s that burns the
                # whole token budget on chain-of-thought before it answers. Crush's
                # `ollama` provider type sends no reasoning control, and Ollama only
                # honours `reasoning_effort: none` (not enable_thinking, Modelfile
                # params, or a system prompt). The typed field rejects "none", so
                # pass it through extra_body, which crush forwards verbatim. Scoped
                # to this model's `options` so discovered models are unaffected.
                options.provider_options.extra_body.reasoning_effort = "none";
              }
            ];
          };
        };
      };
    };
    services = {
      zfs = {
        enable = true;
        # Cache-class data carved out of the snapshotted home dataset. Both
        # directories churn by the tens of GB (nix tarball cache, browser and
        # spotify caches, rootless podman layers) and every byte written there
        # was being retained by auto-snapshots long after deletion — the home
        # dataset held 4x its live size in snapshots. Flat names with explicit
        # mountpoints: nesting under `.local/share` would make `zfs create -p`
        # mount intermediate datasets over the live ~/.local.
        datasets = let
          cacheDataset = mountpoint: {
            properties = {
              inherit mountpoint;
              "com.sun:auto-snapshot" = "false";
            };
            owner = config.user.name;
            group = config.user.group;
            mode = "0700";
          };
        in {
          "zroot/home/${config.user.name}/cache" = cacheDataset "${config.user.home}/.cache";
          "zroot/home/${config.user.name}/containers" = cacheDataset "${config.user.home}/.local/share/containers";
        };
      };
      sudo-rs.enable = true;
      printing = {
        enable = true;
        client = true;
      };
      gpg.enable = true;
      systemd.modules = {
        sshAgent.enable = true;
        tmp-downloads.enable = true;
      };
      ssh = {
        enable = true;
        client = {
          enable = true;
          deployAgent.enable = true;
        };
      };
      yubikey.enable = true;
      syncthing.enable = true;
      cachix.enable = true;
      remote-builder.client = {
        enable = true;
        sshKey = config.sops.secrets.vulcan-builder-ssh-key.path;
      };
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
      };
      airvpn.enable = true;
    };
    # No OCI services run here; registryAuth alone gives interactive
    # podman/skopeo credentials for the forgejo registry. The auth file is
    # user-owned and sits in the user's XDG path, which rootless
    # podman/skopeo already search — no REGISTRY_AUTH_FILE needed.
    linux.oci.registryAuth = {
      enable = true;
      registries."git.3679.space".secret = "registry-auth/forgejo";
      authFile = "${config.user.home}/.config/containers/auth.json";
      owner = config.user.name;
    };
    hardware = {
      zsa.enable = true;
      nvidia = {
        enable = true;
        useOpenDrivers = true;
      };
    };
    desktop = {
      enable = true;
      environment.niri = {
        enable = true;
        outputs = {
          "DP-3" = {
            mode = {
              width = 3840;
              height = 2160;
              refresh = 59.997;
            };
            scale = 1;
            transform.rotation = 0;
            position = {
              x = 0;
              y = 0;
            };
          };
          "DP-1" = {
            mode = {
              width = 3840;
              height = 2160;
              refresh = 59.997;
            };
            scale = 1;
            transform.rotation = 0;
            position = {
              x = 3840;
              y = 0;
            };
          };
        };
      };
      # wt0 (netbird) egresses over this link, so sampling both would count
      # overlay traffic twice.
      noctalia.networkInterface = "enp5s0";
      random-wallpaper = {
        enable = true;
        perDisplay = true;
        token = config.sops.secrets.unsplash.path;
      };
      monitors = ["DP-3" "DP-1"];
      sound.enable = true;
      firefox = {
        enable = true;
        setDefaultPDFViewer = true;
      };
      # misc packages that don't cleanly belong inside of a module
      extraPackages = with pkgs; [
        inputs.rz.packages.${pkgs.stdenv.hostPlatform.system}.with-xz2-bzip2
        feishin
        gh
        moonlight-qt
        signal-desktop
        ungoogled-chromium
      ];
      autostart = {
        enable = true;
        entries = [
          "${pkgs.signal-desktop}/share/applications/signal.desktop"
        ];
      };
    };
    themes.active = "tokyo-night-dark";
  };

  boot = {
    kernelPackages = pkgs.linuxPackages;
    kernelParams = ["nohibernate"];

    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
        windows."windows-11" = {
          title = "Windows 11";
          efiDeviceHandle = "HD0c";
        };
      };

      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
    };

    binfmt.emulatedSystems = ["aarch64-linux"];

    zfs = {
      extraPools = ["zroot"];
      forceImportRoot = false;
    };
  };

  # Snapshots here are an undo buffer for the home dataset, not a backup
  # (single NVMe, no replication). The weekly/monthly tiers only ever pinned
  # months of deleted caches and build trees, so keep just the short window
  # that catches a bad rm/reset. zfstools treats keep=0 as "create none and
  # destroy any that exist", so the old weekly/monthly snapshots go away on
  # the next timer run rather than needing a manual destroy.
  services.zfs.autoSnapshot = {
    daily = 3;
    weekly = 0;
    monthly = 0;
  };

  # TODO move, set defaults
  # TODO separate hardware config
  networking = {
    hostName = "hyperion";
    hostId = "836be91c";
    useNetworkd = true;

    useDHCP = false;

    interfaces.enp5s0.useDHCP = true;

    networkmanager.enable = true;

    # should only get its ip address from the pihole
    dhcpcd.extraConfig = ''
      blacklist 192.168.0.1
    '';
  };

  # TODO implement encrypted home
  # security.pam.zfs = {
  #   enable = true;
  #   homes = "zroot/home";
  # };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # TODO move
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
}
