{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.llama-server;

  modelsDir = "${cfg.dataDir}/models";
  cacheDir = "${cfg.dataDir}/hf";

  # The preset INI is text, so floats have no type of their own and are
  # written as strings.
  settingsType = types.attrsOf (types.oneOf [types.bool types.int types.str]);

  gpuPackage = {
    nvidia = pkgs.llama-cpp.override ({cudaSupport = true;}
      // optionalAttrs (cfg.cudaCapabilities != []) {
        cudaPackages = pkgs.cudaPackages.override (old: {
          config = old.config // {inherit (cfg) cudaCapabilities;};
        });
      });
    amd = pkgs.llama-cpp-rocm;
    intel = pkgs.llama-cpp-vulkan;
  };

  # The router reads `;` and `#` as the start of a comment, and cuts a value
  # there without complaint.
  hasCommentChar = value: isString value && (hasInfix ";" value || hasInfix "#" value);

  sourceKeys = ["model" "m" "hf-repo" "hfr" "hf"];

  modelModule = {
    config,
    name,
    ...
  }: {
    options = {
      settings = mkOption {
        description = ''
          Preset keys written verbatim as this model's `[<id>]` section. Each
          key is a llama-server flag without its leading dashes, and must name
          the model's source (`hf-repo`, `model`, ...).
        '';
        type = settingsType;
        example = {
          hf-repo = "unsloth/Qwen3-14B-GGUF:IQ4_XS";
          temp = "0.7";
        };
      };

      name = mkOption {
        description = "Display name clients show for the model.";
        type = types.str;
        default = name;
        defaultText = literalExpression "the model id";
      };

      contextWindow = mkOption {
        description = ''
          Context window, in tokens, that clients are told about. Has to match
          what the server loads, so it follows `ctx-size`.
        '';
        type = types.ints.positive;
        default = config.settings."ctx-size" or cfg.defaults."ctx-size";
        defaultText = literalExpression ''settings."ctx-size" or defaults."ctx-size"'';
      };

      maxTokens = mkOption {
        description = "Maximum tokens clients request per completion.";
        type = types.ints.positive;
        default = 8192;
      };
    };
  };
in {
  options.modules.services.llama-server = {
    enable = mkEnableOption "llama.cpp model server (router mode)";

    gpu = mkOption {
      description = ''
        GPU type for inference acceleration. Selects the matching llama.cpp
        package variant (CUDA, ROCm or Vulkan).
      '';
      type = types.nullOr (types.enum ["nvidia" "amd" "intel"]);
      default = null;
    };

    cudaCapabilities = mkOption {
      description = ''
        CUDA compute capabilities to build for, as in
        `config.cudaCapabilities`. Empty keeps the nixpkgs default set, which
        compiles every supported architecture; naming just the installed GPU's
        keeps the build short. Only the llama.cpp package is rebuilt, the CUDA
        toolkit still comes from the binary cache.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["8.9"];
    };

    package = mkOption {
      description = ''
        llama.cpp package to use. Defaults to a GPU-specific variant when
        `gpu` is set, or `pkgs.llama-cpp` for CPU-only.
      '';
      type = types.package;
      default =
        if cfg.gpu != null
        then gpuPackage.${cfg.gpu}
        else pkgs.llama-cpp;
      defaultText = literalExpression ''
        {
          nvidia = pkgs.llama-cpp.override { cudaSupport = true; };
          amd = pkgs.llama-cpp-rocm;
          intel = pkgs.llama-cpp-vulkan;
        }.''${cfg.gpu} or pkgs.llama-cpp
      '';
    };

    host = mkOption {
      description = "Address the HTTP API listens on.";
      type = types.str;
      default = "127.0.0.1";
    };

    port = mkOption {
      description = "Port the HTTP API listens on.";
      type = types.port;
      default = 8080;
    };

    dataDir = mkOption {
      description = ''
        Directory holding the model files (`models/`, scanned by the router)
        and the Hugging Face download cache (`hf/`).
      '';
      type = types.str;
      example = "/bulk/llm";
    };

    dataset = mkOption {
      description = ''
        ZFS dataset to create and mount at `dataDir`. When set, the unit also
        refuses to start unless `dataDir` is a mount point.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "bulk/llm";
    };

    defaults = mkOption {
      description = ''
        Preset keys applying to every model, written as the `[*]` section.
        A model's own `settings` override them.
      '';
      type = settingsType;
    };

    models = mkOption {
      description = ''
        Models the router serves, keyed by the id clients request. Each is a
        preset section; the router loads one on first request and unloads it
        when another is asked for.
      '';
      type = types.attrsOf (types.submodule modelModule);
      default = {};
    };

    presetFile = mkOption {
      description = "Generated preset INI passed to `--models-preset`.";
      type = types.package;
      readOnly = true;
    };
  };

  config = mkIf cfg.enable {
    assertions =
      concatLists (mapAttrsToList (id: model: [
          {
            assertion = builtins.match "[a-z0-9][a-z0-9._-]*" id != null;
            message = "llama-server model \"${id}\": the id must match ^[a-z0-9][a-z0-9._-]*$ (the router rewrites section names containing `:`)";
          }
          {
            assertion = !any hasCommentChar (attrValues model.settings);
            message = "llama-server model \"${id}\": a setting value contains `;` or `#`, which the router treats as a comment";
          }
          {
            assertion = any (key: model.settings ? ${key}) sourceKeys;
            message = "llama-server model \"${id}\": settings need a source key (one of ${concatStringsSep ", " sourceKeys})";
          }
        ])
        cfg.models)
      ++ [
        {
          assertion = !any hasCommentChar (attrValues cfg.defaults);
          message = "llama-server defaults: a value contains `;` or `#`, which the router treats as a comment";
        }
        {
          assertion = cfg.cudaCapabilities == [] || cfg.gpu == "nvidia";
          message = "llama-server cudaCapabilities only applies when gpu = \"nvidia\"";
        }
      ];

    # Set per key so a host can add or override single entries; a whole-attrset
    # option default would be replaced by the first definition.
    modules.services.llama-server.defaults = mapAttrs (_: mkDefault) {
      ctx-size = 32768;
      # Quantizing the V cache requires flash attention.
      flash-attn = "on";
      cache-type-k = "q8_0";
      cache-type-v = "q8_0";
      # Hugging Face downloads are always listed; this hides the ones a preset
      # already covers, so a model appears under its preset id only.
      dedup-cache-models = 1;
    };

    modules.services.llama-server.presetFile = pkgs.writeText "llama-server-presets.ini" (
      generators.toINI {} ({"*" = cfg.defaults;} // mapAttrs (_: model: model.settings) cfg.models)
    );

    modules.services.zfs.datasets = mkIf (cfg.dataset != null) {
      ${cfg.dataset} = {
        properties = {
          mountpoint = cfg.dataDir;
          # Model files are large and read sequentially.
          recordsize = "1M";
          "com.sun:auto-snapshot" = "false";
        };
        owner = config.user.name;
        group = config.user.group;
        mode = "0755";
      };
    };

    systemd.user.services.llama-server = {
      description = "llama.cpp model server (router mode)";
      # Beside modelsDir, not inside it: the router treats every subdirectory
      # of --models-dir as a model.
      environment.LLAMA_CACHE = cacheDir;
      # With the dataset unmounted the directories below would be created on
      # the root filesystem and the router would download models into it.
      unitConfig = optionalAttrs (cfg.dataset != null) {
        AssertPathIsMountPoint = cfg.dataDir;
      };
      serviceConfig = {
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${escapeShellArgs [modelsDir cacheDir]}";
        ExecStart = escapeShellArgs [
          (getExe' cfg.package "llama-server")
          "--host"
          cfg.host
          "--port"
          (toString cfg.port)
          "--models-dir"
          modelsDir
          "--models-preset"
          cfg.presetFile
          "--models-max"
          "1"
          "--no-webui"
        ];
        Restart = "on-failure";
        # llama.cpp gives each model process 10 s to exit before SIGKILL. The
        # default control-group KillMode makes stop wait for all of them, so
        # a stopped unit has released the GPU.
        TimeoutStopSec = 30;
      };
    };
  };
}
