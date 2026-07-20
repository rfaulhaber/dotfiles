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

  nix.settings = {
    substituters = ["https://install.determinate.systems"];
    trusted-public-keys = ["cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="];
  };

  modules = {
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
        };
      };
    };
    services = {
      zfs.enable = true;
      sudo-rs.enable = true;
      observability-agent = {
        enable = true;
        prometheus.openFirewall = true;
        loki.extraLabels.role = "ci";
      };
      ollama = {
        enable = true;
        home = "/apps/ollama";
        modelsDir = "/mnt/llm/models";
        gpu = "intel";
        openFirewall = true;
        models = ["qwen3:30b-a3b" "qwen3:14b" "gpt-oss:20b" "gemma4:26b"];
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
        interface = "enp4s0";
        secretKeyFile = config.sops.secrets.nix-cache.path;
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
      extraPools = ["zroot"];
      forceImportRoot = false;
    };
  };

  networking = {
    hostName = "vulcan";
    hostId = "896980a5";

    useDHCP = false;
    interfaces.enp4s0.useDHCP = true;

    # Pin atlas to its LAN IPv4 so NFS mounts use the local path. Without
    # this, DNS returns a public AAAA record and atlas's NFS exports (keyed
    # on 192.168.0.105) reject the IPv6 source.
    hosts."192.168.0.3" = ["atlas"];

    firewall.enable = true;
  };

  sops = {
    secrets = {
      "newt/id" = {};
      "newt/secret" = {};
      "forgejo-runner/token" = {};
      "codeberg-runner/token" = {};
    };
    templates = {
      "forgejo-runner-env".content = ''
        FORGEJO_TOKEN=${config.sops.placeholder."forgejo-runner/token"}
      '';
      "codeberg-runner-env".content = ''
        FORGEJO_TOKEN=${config.sops.placeholder."codeberg-runner/token"}
      '';
    };
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
