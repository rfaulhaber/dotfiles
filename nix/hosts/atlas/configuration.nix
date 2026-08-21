{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../../modules
    ./hardware-configuration.nix
    ./oci.nix
    inputs.determinate.nixosModules.default
  ];

  modules = {
    nix.bigHost = true;
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
          "netbird/setup-key" = {};
        };
      };
    };
    services = {
      zfs = {
        enable = true;
        datasets = {
          # LLM model store shared to vulcan over NFS (ollama blobs are
          # multi-GB sequential files, hence the large recordsize).
          "data/llm/models".properties = {
            mountpoint = "/data/llm/models";
            recordsize = "1M";
          };
        };
        encryptedDatasets = {
          filebrowser-files = {
            dataset = "data/apps/filebrowser/files";
            keyFile = config.sops.secrets."filebrowser/zfs-key".path;
            consumers = ["podman-filebrowser.service"];
          };
          sync = {
            dataset = "data/files/sync";
            keyFile = config.sops.secrets."sync/zfs-key".path;
            consumers = ["podman-syncthing.service"];
          };
          org = {
            dataset = "data/files/org";
            keyFile = config.sops.secrets."org/zfs-key".path;
            consumers = ["podman-syncthing.service"];
          };
        };
      };
      sudo-rs.enable = true;
      observability-agent = {
        enable = true;
        prometheus = {
          openFirewall = true;
          interface = "eno2";
        };
      };
      systemd.modules = {
        updatedb.enable = true;
      };
      printing = {
        enable = true;
        server = true;
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          extraConfig = ''
            MaxStartups 30:30:60
          '';
        };
      };
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
      };
      samba.serve = {
        enable = true;
        subnet = "192.168.0.";
        interface = "eno2";
        shares.roms = {
          path = "/data/games/roms/roms";
          comment = "ROM library";
          readOnly = true;
          guestOk = true;
        };
      };
      nfs.serve = {
        enable = true;
        interface = "eno2";
        # root_squash on purpose: everything on vulcan writes as uid 1000
        # (jellyfin/plex) or 994 (ollama), never root — verified against
        # on-disk ownership 2026-08. vulcan's CI trust domain reaches host
        # root there; squashing keeps a rogue job from writing as root
        # into these datasets.
        exports = {
          movies = {
            path = "/data/movies";
            clients = "192.168.0.105(rw,sync,no_subtree_check,root_squash)";
          };
          tv = {
            path = "/data/tv";
            clients = "192.168.0.105(rw,sync,no_subtree_check,root_squash)";
          };
          llm-models = {
            path = "/data/llm/models";
            clients = "192.168.0.105(rw,sync,no_subtree_check,root_squash)";
          };
          # ro: the ROM library is canonical here; wolf sessions on vulcan
          # only ever read it (saves/states live in vulcan's app state).
          roms = {
            path = "/data/games/roms";
            clients = "192.168.0.105(ro,sync,no_subtree_check,root_squash)";
          };
        };
      };
      bandcamp-import = {
        enable = true;
        zfsDataset = "data/import/bandcamp";
      };
      extract-embedded-subs = {
        enable = true;
        linger = true;
      };
    };

    # NOTE: the open drivers do not work on atlas (GTX 1050 Ti, Pascal)
    hardware.nvidia = {
      enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    };

    themes.active = "tokyo-night-dark";
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.device = "nodev";
    };

    kernelPackages = pkgs.linuxPackages;
    kernelParams = ["nohibernate"];

    zfs = {
      extraPools = ["system" "data"];
      forceImportRoot = false;
    };
  };

  # Exporting a path before its dataset is mounted silently drops the export
  # until a manual `exportfs -r`; order the server behind dataset creation.
  systemd.services.nfs-server.after = ["zfs-manage-datasets.service"];

  # Without a scope avahi publishes across every podman bridge and veth pair
  # (~40 interfaces here) plus the netbird overlay; the printer only belongs on
  # the LAN.
  services.avahi.allowInterfaces = ["eno2"];

  sops.secrets = {
    "filebrowser/zfs-key" = {
      format = "binary";
      sopsFile = ./secrets/filebrowser-zfs-key;
    };
    "sync/zfs-key" = {
      format = "binary";
      sopsFile = ./secrets/sync-zfs-key;
    };
    "org/zfs-key" = {
      format = "binary";
      sopsFile = ./secrets/org-zfs-key;
    };
  };

  networking = {
    hostName = "atlas";
    hostId = "d6acc614";

    useDHCP = false;

    # Privacy addresses rotate the v6 address every few minutes; servers want a
    # stable, predictable address.
    tempAddresses = "disabled";

    interfaces = {
      eno1.useDHCP = true;
      eno2.useDHCP = true;
    };

    # should only get its static ip address from the pihole
    dhcpcd.extraConfig = ''
      blacklist 192.168.0.1
    '';

    firewall = {
      enable = true;
      # required for pihole
      allowedTCPPorts = [8085 80 53 67];
      allowedUDPPorts = [53 67 68 546 547];
      extraCommands = ''
        iptables -I INPUT 1 -p tcp -m tcp --dport 4711 -i lo -j ACCEPT
        iptables -I INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
      '';
    };
  };
}
