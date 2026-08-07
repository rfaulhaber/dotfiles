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

  # TODO move, make reusable
  nix.settings = {
    substituters = [
      "https://install.determinate.systems"
      "http://vulcan.lan:4965"
      "https://nixos-raspberrypi.cachix.org"
    ];
    trusted-public-keys = [
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
      "vulcan.lan-1:Zu8N+6EtaIeDTyCVpR15uvIYYByZqMmd8W09vu8GKl8="
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
    ];
  };

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
    programs = {
      btop.enable = true;
      emacs = {
        enable = true;
        package = pkgs.emacs-git;
      };
      kitty.enable = true;
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
      zfs.enable = true;
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
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
      };
      airvpn.enable = true;
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
      extraPackages = with pkgs; [
        feishin
        inputs.rz.packages.${pkgs.stdenv.hostPlatform.system}.with-xz2-bzip2
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
    desktop.fuzzel.colorOverrides = {
      # base16's base08 ("red") is a pale lavender here, leaving matched chars
      # nearly identical to the grey selection highlight. base0F is the scheme's
      # salmon-red — saturated enough to read on both the dark background and the
      # light selection row.
      match = config.modules.themes.colors.base0F;
      selection-match = config.modules.themes.colors.base0F;
    };

    themes = {
      active = "tokyo-night-dark";
      # tokyo-night-dark's base0A ("yellow") slot is actually a cyan-blue, which
      # turned resolved external commands blue. Restore a true amber globally.
      overrides.yellow = "#e0af68";
    };
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
