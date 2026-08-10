{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.ollama;

  intelVulkanIcd = "/run/opengl-driver/share/vulkan/icd.d/intel_icd.x86_64.json";

  gpuPackage = {
    nvidia = pkgs.ollama-cuda;
    amd = pkgs.ollama-rocm;
    intel = pkgs.ollama-vulkan;
  };

  # Repairing ownership is conditional because it has to be a no-op on an
  # NFS-mounted modelsDir: exports are served root_squash, so a root-run
  # chown is denied there even when the directory is already owned by the
  # ollama uid and the service writes to it fine. Ownership across the mount
  # is the server's to set; the client can only check it.
  ensureOwnership = pkgs.writeShellScript "ollama-ensure-ownership" ''
    set -uo pipefail
    for dir in "$@"; do
      if ! ${pkgs.coreutils}/bin/mkdir -p "$dir"; then
        exit 1
      fi

      owner=$(${pkgs.coreutils}/bin/stat -c '%U:%G' "$dir")
      if [ "$owner" = "ollama:ollama" ]; then
        continue
      fi

      if ! ${pkgs.coreutils}/bin/chown ollama:ollama "$dir"; then
        echo "cannot take ownership of $dir (owned by $owner)" >&2
        echo "if it is an NFS mount, the export is root_squash — chown it on the server" >&2
        exit 1
      fi
    done
  '';
in {
  options.modules.services.ollama = {
    enable = mkEnableOption "Ollama LLM server";

    package = mkOption {
      description = ''
        Ollama package to use. Defaults to a GPU-specific variant when
        `gpu` is set (`pkgs.ollama-cuda` / `-rocm` / `-vulkan`), or
        `pkgs.ollama` for CPU-only.
      '';
      type = types.package;
      default =
        if cfg.gpu != null
        then gpuPackage.${cfg.gpu}
        else pkgs.ollama;
      defaultText = literalExpression ''
        {
          nvidia = pkgs.ollama-cuda;
          amd = pkgs.ollama-rocm;
          intel = pkgs.ollama-vulkan;
        }.''${cfg.gpu} or pkgs.ollama
      '';
    };

    host = mkOption {
      description = "Address the HTTP API listens on.";
      type = types.str;
      default = "0.0.0.0";
    };

    port = mkOption {
      description = "Port the HTTP API listens on.";
      type = types.port;
      default = 11434;
    };

    home = mkOption {
      description = ''
        Home directory for ollama state (manifests, history, cache).
        Created and chowned to the ollama user at unit start so a freshly
        created ZFS dataset ends up with the correct ownership.
      '';
      type = types.str;
      default = "/var/lib/ollama";
    };

    modelsDir = mkOption {
      description = ''
        Directory holding model blobs and manifests. Often points at
        NFS-mounted storage; the systemd unit waits for this path to be
        mounted via `RequiresMountsFor` before starting, and expects the
        server to have set ownership to the ollama uid already.
      '';
      type = types.str;
      default = "${cfg.home}/models";
      defaultText = literalExpression ''"''${cfg.home}/models"'';
    };

    gpu = mkOption {
      description = ''
        GPU type for inference acceleration. Selects the matching ollama
        package variant; for `intel`, also pins the Mesa Vulkan ICD via
        VK_ICD_FILENAMES so the loader doesn't fall back to llvmpipe.
      '';
      type = types.nullOr (types.enum ["nvidia" "amd" "intel"]);
      default = null;
    };

    openFirewall = mkOption {
      description = "Whether to open the API port in the host firewall.";
      type = types.bool;
      default = false;
    };

    models = mkOption {
      description = ''
        Models to pull on service start. Forwarded to
        `services.ollama.loadModels`, which runs an
        `ollama-model-loader.service` to issue `ollama pull` in the
        background.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["qwen2.5:14b"];
    };

    extraEnvironment = mkOption {
      description = "Additional environment variables for the ollama process.";
      type = types.attrsOf types.str;
      default = {};
    };

    zfs = {
      enable = mkEnableOption "ZFS dataset management for the ollama home directory";
      pool = mkOption {
        type = types.str;
        description = "ZFS pool to create the dataset under.";
        example = "zroot";
      };
      properties = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "ZFS properties applied to the dataset.";
      };
    };
  };

  config = mkIf cfg.enable {
    services.ollama = {
      enable = true;
      package = cfg.package;
      host = cfg.host;
      port = cfg.port;
      home = cfg.home;
      modelsDir = cfg.modelsDir;
      loadModels = cfg.models;
      openFirewall = cfg.openFirewall;
      # Static user/group so the (possibly ZFS-mounted) home directory
      # can be chowned deterministically. DynamicUser doesn't combine
      # cleanly with a custom home path on a managed dataset.
      user = "ollama";
      group = "ollama";
      environmentVariables =
        cfg.extraEnvironment
        // optionalAttrs (cfg.gpu == "intel") {
          VK_ICD_FILENAMES = intelVulkanIcd;
        };
    };

    systemd.services.ollama = mkMerge [
      {
        unitConfig.RequiresMountsFor = [cfg.modelsDir];
        # A freshly created ZFS dataset comes up root-owned and appears after
        # activation-time tmpfiles has already run, so a tmpfiles chown lands
        # on the directory the dataset is then mounted over. Fix ownership at
        # unit start instead; the `+` prefix runs as root.
        serviceConfig.ExecStartPre = [
          "+${ensureOwnership} ${cfg.home} ${cfg.modelsDir}"
        ];
      }
      (mkIf cfg.zfs.enable {
        after = ["zfs-manage-datasets.service"];
        requires = ["zfs-manage-datasets.service"];
      })
    ];

    modules.services.zfs.datasets = mkIf cfg.zfs.enable {
      "${cfg.zfs.pool}${cfg.home}" = {
        properties = {mountpoint = cfg.home;} // cfg.zfs.properties;
      };
    };
  };
}
