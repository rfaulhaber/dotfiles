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

  serverUrl = "http://${cfg.host}:${toString cfg.port}";
  baseUrl = "${serverUrl}/v1";

  providerName = "llama.cpp (local)";

  agentNames = ["crush" "opencode" "pi" "llm"];

  # llm has no module of its own, so only its flag decides.
  isActive = agent: cfg.agents.${agent} && (agent == "llm" || config.modules.programs.${agent}.enable);

  # The preset INI is text, so floats have no type of their own and are
  # written as strings.
  settingsType = types.attrsOf (types.oneOf [types.bool types.int types.str]);

  llm-server = lib.my.writeNushellScriptBin pkgs "llm-server" (
    # The script resolves systemctl and journalctl from PATH and takes its
    # endpoint from the environment, so it runs and debugs unchanged from
    # bin/. This preamble is all the packaged copy adds: the address this
    # module configures.
    ''
      $env.LLM_SERVER_URL = ($env.LLM_SERVER_URL? | default "${serverUrl}")
    ''
    + builtins.readFile "${config.dotfiles.binDir}/llm-server.nu"
  );

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

    # Each enabled client gets a `llama-local` provider listing `models`. The
    # id is not `llama.cpp` because pi reserves that for its own built-in
    # provider. A fixed set of options rather than an attrset of booleans, so a
    # typo such as `agents.crsuh` fails evaluation instead of doing nothing.
    agents = genAttrs agentNames (agent: mkEnableOption "the ${agent} client's llama-local provider");
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

    user.packages = [llm-server];

    # crush's `llamacpp` type would double the `/v1` prefix in its model
    # lookups, so this is plain OpenAI-compatible. Discovered models arrive
    # with a context window of 0, which turns off auto-summarize, so the
    # presets are listed with their real figures. Discovery only runs on its
    # own when `models` is empty, hence `discover_models`, which keeps the
    # router's Hugging Face cache entries in the picker. Defined only here:
    # `providers` is shallow-merged per provider id, so a second definition
    # would drop keys.
    modules.programs.crush.providers = mkIf (isActive "crush") {
      llama-local = {
        name = providerName;
        type = "openai-compat";
        base_url = baseUrl;
        discover_models = true;
        models =
          mapAttrsToList (id: model: {
            inherit id;
            inherit (model) name;
            context_window = model.contextWindow;
            default_max_tokens = model.maxTokens;
          })
          cfg.models;
      };
    };

    home.programs.opencode.settings = mkIf (isActive "opencode") {
      provider.llama-local = {
        npm = "@ai-sdk/openai-compatible";
        name = providerName;
        options.baseURL = baseUrl;
        models =
          mapAttrs (_: model: {
            inherit (model) name;
            tool_call = true;
            # Without a limit opencode assumes a 200k context.
            limit = {
              context = model.contextWindow;
              output = model.maxTokens;
            };
          })
          cfg.models;
      };
    };

    home.programs.pi-coding-agent.models = mkIf (isActive "pi") {
      providers.llama-local = {
        name = providerName;
        inherit baseUrl;
        api = "openai-completions";
        # pi hides providers that have no key. llama-server never checks it.
        apiKey = "local";
        # Mirrors pi's built-in llama.cpp provider.
        compat = {
          supportsStore = false;
          supportsDeveloperRole = false;
          supportsReasoningEffort = false;
          maxTokensField = "max_tokens";
        };
        models =
          mapAttrsToList (id: model: {
            inherit id;
            inherit (model) name contextWindow maxTokens;
          })
          cfg.models;
      };
    };

    # With `api_base` set llm sends a placeholder key, so no `api_key_name`.
    home.configFile = mkIf (isActive "llm") {
      "io.datasette.llm/extra-openai-models.yaml".source = (pkgs.formats.yaml {}).generate "extra-openai-models.yaml" (
        mapAttrsToList (id: _: {
          model_id = id;
          model_name = id;
          api_base = baseUrl;
          supports_tools = true;
        })
        cfg.models
      );
    };
  };
}
