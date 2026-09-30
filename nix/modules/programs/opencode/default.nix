{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.programs.opencode;
  claude = config.modules.programs.claude;
  mcp = config.modules.programs.mcp;
  inherit (inputs.home-manager.lib.hm) dag;

  # OpenCode provider id -> sops secret name, restricted to providers that
  # actually have a secret wired up.
  apiKeySecrets = filterAttrs (_: secret: secret != null) {
    anthropic = cfg.anthropicApiKeySecret;
    openrouter = cfg.openrouterApiKeySecret;
  };

  # OpenCode expands `{file:...}` in the config text before parsing it
  # (trimmed and JSON-escaped), so the rendered opencode.json only ever names
  # the secret's path and can live in the store. A key set here outranks both
  # `opencode auth login` and the environment, and a missing file is a hard
  # startup error rather than a fallback to either.
  providers =
    mapAttrs (_: secretName: {
      options.apiKey = "{file:${config.sops.secrets.${secretName}.path}}";
    })
    apiKeySecrets;

  # Header values take `{file:...}` like `providers` does. A remote server is
  # otherwise treated as a possible OAuth server: a 401 starts discovery and
  # dynamic client registration, which turns an expired static token into a
  # misleading "needs authentication" prompt instead of a plain failure.
  mcpServers =
    mapAttrs (
      _: server:
        if server.command != null
        then {
          type = "local";
          command = [server.command] ++ server.args;
        }
        else
          {
            type = "remote";
            inherit (server) url;
          }
          // optionalAttrs (server.headers != {}) {inherit (server) headers;}
          // optionalAttrs (server.secretHeaders != {}) {oauth = false;}
    )
    (mcp.lib.serversFor "opencode" ({path, ...}: "{file:${path}}"));

  # Claude's `Bash(<glob>)` rules carry over verbatim: OpenCode's patterns are
  # likewise anchored globs over the command text, and it splits compound
  # commands and substitutions into their simple commands, checking each.
  bashGlobs = rules:
    concatMap (
      rule: let
        m = builtins.match "Bash\\((.*)\\)" rule;
      in
        optional (m != null) (head m)
    )
    rules;
  denyGlobs = bashGlobs claude.deniedTools;
  allowGlobs = subtractLists denyGlobs (bashGlobs claude.allowedTools);

  # The last matching rule wins, and OpenCode's own default for bash is
  # allow, so the allowlist only means something on top of an explicit `ask`
  # baseline. Redirect targets are never path-checked, so a redirected
  # command is pulled back to a prompt; the redirect only shows up in the
  # pattern of a command that stands alone, so a redirect on the tail of a
  # pipeline of allowed commands still slips through. Denies go last so they
  # win over any allow, as in Claude.
  bashPermissions =
    {"*" = "ask";}
    // genAttrs allowGlobs (_: dag.entryAfter ["*"] "allow")
    // {"*>*" = dag.entryAfter (["*"] ++ allowGlobs) "ask";}
    // genAttrs denyGlobs (_: dag.entryAfter (["*" "*>*"] ++ allowGlobs) "deny");
in {
  options.modules.programs.opencode = {
    enable = mkEnableOption "opencode, the open source AI coding agent";

    package = mkOption {
      type = types.package;
      default = pkgs.opencode;
      description = "The opencode package to install.";
    };

    anthropicApiKeySecret = mkOption {
      type = types.nullOr types.str;
      default = "anthropic-api-key";
      description = ''
        Name of the sops secret containing the Anthropic Console API key. Must
        be declared in `modules.programs.sops.secrets.<name>`. Set to `null` to
        skip sops integration entirely (e.g. when relying on
        `$ANTHROPIC_API_KEY` or `opencode auth login` instead).
      '';
    };

    openrouterApiKeySecret = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "openrouter-opencode-api-key";
      description = ''
        Name of the sops secret containing the OpenRouter API key. Must be
        declared in `modules.programs.sops.secrets.<name>`. Defaults to `null`
        (opt-in). OpenRouter is in OpenCode's built-in catalog, so the key is
        all it needs.
      '';
    };

    model = mkOption {
      type = types.str;
      # The newest Opus in opencode 1.18.31's bundled catalog. An id missing
      # from it fails with an opaque UnknownError until the first online
      # refresh has populated ~/.cache/opencode/models.json.
      default = "anthropic/claude-opus-5";
      description = ''
        Model for the main agent loop, as `<provider>/<model id>`. OpenRouter
        ids nest the vendor, e.g. `openrouter/anthropic/claude-opus-5.5`.
      '';
    };

    smallModel = mkOption {
      type = types.str;
      default = "anthropic/claude-haiku-4-5";
      description = ''
        Model for lightweight tasks such as session titles and summaries, as
        `<provider>/<model id>`.
      '';
    };

    reuseClaudeConfig = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Drive opencode from the Claude Code configuration in this repo:

        - `config/claude/CLAUDE.md` is installed as
          `~/.config/opencode/AGENTS.md`, which opencode reads in place of
          `~/.claude/CLAUDE.md`.
        - `config/claude/skills` becomes opencode's global skills directory on
          hosts where the claude module isn't already providing
          `~/.claude/skills`, which opencode scans natively.
        - The `Bash(...)` rules in `modules.programs.claude.allowedTools` and
          `deniedTools` become opencode's `permission.bash` rules, on top of a
          prompt for every other command.

        Everything else opencode should know about — further providers,
        permissions for other tools, `tui`, agents — goes straight into
        `home.programs.opencode`; home-manager deep-merges it with what this
        module derives.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions =
      mapAttrsToList (providerId: secretName: {
        assertion = config.sops.secrets ? ${secretName};
        message = ''
          modules.programs.opencode.${providerId}ApiKeySecret is set to "${secretName}"
          but no matching secret is declared in modules.programs.sops.secrets.
        '';
      })
      apiKeySecrets;

    home.programs.opencode = {
      enable = true;
      inherit (cfg) package;

      settings = mkMerge [
        {
          inherit (cfg) model;
          small_model = cfg.smallModel;
        }
        (mkIf (providers != {}) {provider = providers;})
        (mkIf (mcpServers != {}) {mcp = mcpServers;})
        (mkIf cfg.reuseClaudeConfig {
          permission.bash = bashPermissions;
        })
      ];

      context = mkIf cfg.reuseClaudeConfig ../../../../config/claude/CLAUDE.md;

      # Opencode already scans ~/.claude/skills, so while the claude module
      # installs that tree a second copy under ~/.config/opencode/skills
      # would only shadow it.
      skills =
        mkIf (cfg.reuseClaudeConfig && !claude.enable)
        ../../../../config/claude/skills;
    };
  };
}
