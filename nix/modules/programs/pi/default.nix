{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.pi;

  # Pi provider id -> sops secret name, restricted to providers that
  # actually have a secret wired up.
  apiKeySecrets = filterAttrs (_: secret: secret != null) {
    anthropic = cfg.anthropicApiKeySecret;
    openrouter = cfg.openrouterApiKeySecret;
  };
in {
  options.modules.programs.pi = {
    enable = mkEnableOption "pi, the minimal terminal coding agent";

    package = mkOption {
      type = types.package;
      default = pkgs.pi-coding-agent;
      description = "The pi package to install.";
    };

    anthropicApiKeySecret = mkOption {
      type = types.nullOr types.str;
      default = "anthropic-api-key";
      description = ''
        Name of the sops secret containing the Anthropic Console API key. Must
        be declared in `modules.programs.sops.secrets.<name>`. Set to `null` to
        skip sops integration entirely (e.g. when relying on
        `$ANTHROPIC_API_KEY` or a `/login` subscription instead).
      '';
    };

    openrouterApiKeySecret = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "openrouter-pi-api-key";
      description = ''
        Name of the sops secret containing the OpenRouter API key. Must be
        declared in `modules.programs.sops.secrets.<name>`. Defaults to `null`
        (opt-in). OpenRouter is one of pi's built-in providers, so the key is
        all it needs.
      '';
    };

    defaultProvider = mkOption {
      type = types.str;
      default = "anthropic";
      description = "Provider pi starts new sessions with.";
    };

    defaultModel = mkOption {
      type = types.str;
      default = "claude-opus-5-5";
      description = ''
        Model id, as named in `defaultProvider`'s catalog, that pi starts new
        sessions with. OpenRouter ids carry the upstream vendor prefix, e.g.
        `anthropic/claude-opus-5.5`.
      '';
    };

    reuseClaudeConfig = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Drive pi from the Claude Code configuration in this repo:

        - `config/claude/CLAUDE.md` is installed as `~/.pi/agent/AGENTS.md`,
          pi's global context file.
        - `config/claude/skills` is linked into `~/.pi/agent/skills`. Pi does
          not scan `~/.claude/skills`, so this happens whether or not the
          claude module is enabled.

        Pi has no tool-approval system of its own, so the claude module's
        `Bash(...)` allowlist has nothing to attach to here.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions =
      mapAttrsToList (providerId: secretName: {
        assertion = config.sops.secrets ? ${secretName};
        message = ''
          modules.programs.pi.${providerId}ApiKeySecret is set to "${secretName}"
          but no matching secret is declared in modules.programs.sops.secrets.
        '';
      })
      apiKeySecrets;

    home.programs.pi-coding-agent = {
      enable = true;
      inherit (cfg) package;

      # Pi runs `!command` values itself, fresh on every request, so
      # models.json only names the secret's path and a rotated key is picked
      # up without a restart. The keys go here rather than in auth.json
      # because pi writes auth.json itself (`/login`, OAuth refresh) and a
      # read-only store link would break that; a credential stored there
      # takes precedence over these.
      models = mkIf (apiKeySecrets != {}) {
        providers =
          mapAttrs (_: secretName: {
            apiKey = "!cat ${escapeShellArg config.sops.secrets.${secretName}.path}";
          })
          apiKeySecrets;
      };

      settings = {
        inherit (cfg) defaultProvider defaultModel;

        # settings.json is a read-only store link. With no version recorded,
        # pi treats every launch as a fresh install and tries to write one.
        lastChangelogVersion = mkDefault (getVersion cfg.package);
      };

      context = mkIf cfg.reuseClaudeConfig ../../../../config/claude/CLAUDE.md;
    };

    home.file.".pi/agent/skills" = mkIf cfg.reuseClaudeConfig {
      source = ../../../../config/claude/skills;
      recursive = true;
    };
  };
}
