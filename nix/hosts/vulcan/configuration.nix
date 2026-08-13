{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.disko.nixosModules.disko
    ./disko.nix
    ./hardware.nix
    inputs.determinate.nixosModules.default
    ./oci.nix
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  modules = {
    nix = {
      bigHost = true;
      # The CI runners build every x86_64 host's closure through this host's
      # nix daemon, so the daemon needs the shared list. (The Forgejo
      # workflows composed this per-job in configure-nix.nu; native runners
      # inherit the daemon's config instead.)
      substituters.enable = true;
    };
    programs = {
      btop.enable = true;
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      sops = {
        enable = true;
        keyFile = null;
        secrets = {
          nix-cache = {};
          "netbird/setup-key" = {};
          "github-runner/token" = {};
        };
      };
    };
    services = {
      zfs.enable = true;
      sudo-rs.enable = true;
      observability-agent = {
        enable = true;
        prometheus = {
          openFirewall = true;
          interface = "lan0";
        };
        loki.extraLabels.role = "ci";
      };
      ollama = {
        enable = true;
        home = "/apps/ollama";
        modelsDir = "/mnt/llm/models";
        gpu = "intel";
        openFirewall = true;
        models = ["qwen3:30b-a3b" "qwen3:14b" "gpt-oss:20b" "gemma4:26b"];
        # Default context is 4096, which is too small for coding use.
        # Raise the server-wide default; clients can still override
        # per-request via num_ctx.
        extraEnvironment.OLLAMA_CONTEXT_LENGTH = "32768";
        zfs = {
          enable = true;
          pool = "zroot";
        };
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 13308;
        };
      };
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
      };
      nix-cache = {
        enable = true;
        port = 4965;
        interface = "lan0";
        secretKeyFile = config.sops.secrets.nix-cache.path;
      };
      github-runner = {
        enable = true;
        url = "https://github.com/rfaulhaber/dotfiles";
        tokenFile = config.sops.secrets."github-runner/token".path;
        # Four instances so the x86_64 half of the build matrix runs
        # fully parallel, matching the old Forgejo runner's capacity.
        count = 4;
        extraLabels = ["nix"];
      };
      nfs.mount = {
        enable = true;
        mounts = {
          "/mnt/media/movies" = {
            server = "atlas";
            path = "/data/movies";
          };
          "/mnt/media/tv" = {
            server = "atlas";
            path = "/data/tv";
          };
          "/mnt/llm/models" = {
            server = "atlas";
            path = "/data/llm/models";
          };
          # ROM library for wolf's retro emulators: bind /mnt/games/roms/roms
          # into emulator sessions at /home/retro/ROMs via the app's mounts in
          # config.toml (a subpath — /home/retro itself is taken by app state,
          # see wolf issue #461 above).
          "/mnt/games/roms" = {
            server = "atlas";
            path = "/data/games/roms";
          };
        };
      };
    };

    hardware.intel-gpu.enable = true;

    themes.active = "tokyo-night-dark";
  };

  boot = {
    binfmt.emulatedSystems = ["aarch64-linux"];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages;
    kernelParams = ["nohibernate"];
    zfs = {
      extraPools = ["zroot" "store"];
      forceImportRoot = false;
    };
  };

  networking = {
    hostName = "vulcan";
    hostId = "896980a5";

    useDHCP = false;
    # The NIC shares the B550 chipset bridge with M2_2, so PCI-slot-derived
    # names shift whenever a device lands there — installing the store NVMe
    # renamed enp4s0 to enp5s0 and silently orphaned every reference. lan0 is
    # pinned to the NIC's MAC via systemd.network.links below.
    interfaces.lan0.useDHCP = true;
    # Leave addresses configured when dhcpcd stops, so a daemon restart never
    # severs the SSH session a deploy is riding on.
    dhcpcd.persistent = true;

    # Pin atlas to its LAN IPv4 so NFS mounts use the local path. Without
    # this, DNS returns a public AAAA record and atlas's NFS exports (keyed
    # on 192.168.0.105) reject the IPv6 source.
    hosts."192.168.0.3" = ["atlas"];

    firewall.enable = true;
  };

  # Stable name for the onboard Realtek NIC, keyed on hardware identity
  # instead of PCI topology. Applied by udev at device add — a rename takes
  # effect on reboot, not at activation.
  systemd.network.links."10-lan0" = {
    matchConfig.PermanentMACAddress = "9c:6b:00:d7:3d:f1";
    linkConfig.Name = "lan0";
  };

  sops = {
    secrets = {
      "newt/id" = {};
      "newt/secret" = {};
      "forgejo-runner/token" = {};
    };
    templates = {
      "forgejo-runner-env".content = ''
        FORGEJO_TOKEN=${config.sops.placeholder."forgejo-runner/token"}
      '';
    };
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
