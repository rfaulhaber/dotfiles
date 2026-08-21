{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.crush;
  claude = config.modules.programs.claude;

  # Crush provider id -> sops secret name, restricted to providers that
  # actually have a secret wired up.
  apiKeySecrets = filterAttrs (_: secret: secret != null) {
    anthropic = cfg.anthropicApiKeySecret;
    openrouter = cfg.openrouterApiKeySecret;
  };

  # Crush runs `$(command)` substitutions in config values at startup, so the
  # rendered crush.json only ever names the secret's path and can live in the
  # store like any other home-manager file. `(... or {})` keeps the provider
  # entry even when the host didn't pre-declare it in `providers`.
  providers =
    foldl' (
      acc: providerId:
        acc
        // {
          ${providerId} =
            (acc.${providerId} or {})
            // {
              api_key = "$(cat ${config.sops.secrets.${apiKeySecrets.${providerId}}.path})";
            };
        }
    )
    cfg.providers
    (attrNames apiKeySecrets);

  # Claude's `Bash(<glob>)` rules, as shell `case` patterns. Each literal
  # segment is single-quoted so `#`, spaces and the like can't be
  # reinterpreted; only the `*` wildcards stay unquoted.
  bashGlobs = rules:
    concatMap (
      rule: let
        m = builtins.match "Bash\\((.*)\\)" rule;
      in
        optional (m != null) (head m)
    )
    rules;
  toCasePattern = glob:
    concatStringsSep "*" (map (
      seg: optionalString (seg != "") "'${replaceStrings ["'"] ["'\\''"] seg}'"
    ) (splitString "*" glob));
  casePatterns = rules: concatStringsSep "|" (map toCasePattern (bashGlobs rules));

  allowPatterns = casePatterns claude.allowedTools;
  denyPatterns = casePatterns claude.deniedTools;

  # Crush's permission config can only name whole tools, so the curated Bash
  # rules from the claude module are applied through a PreToolUse hook
  # instead: print an `allow` decision to skip the prompt, exit 2 to deny,
  # stay silent to fall through to the normal prompt. The rules are globs over
  # a single command line, so anything that could chain, substitute or
  # redirect is never auto-approved — matching only the leading command would
  # wave through `git status; rm -rf ~`.
  bashPermissionHook = pkgs.writeShellScript "crush-claude-bash-permissions" ''
    set -u
    cmd="''${CRUSH_TOOL_INPUT_COMMAND-}"
    [ -n "$cmd" ] || exit 0
    ${optionalString (denyPatterns != "") ''
      case "$cmd" in
        ${denyPatterns})
          printf 'denied by modules.programs.claude.deniedTools\n' >&2
          exit 2
          ;;
      esac
    ''}
    nl='
    '
    case "$cmd" in
      *';'* | *'&'* | *'|'* | *'`'* | *'$('* | *'<'* | *'>'* | *"$nl"*) exit 0 ;;
    esac
    ${optionalString (allowPatterns != "") ''
      case "$cmd" in
        ${allowPatterns}) printf '{"decision":"allow"}\n' ;;
      esac
    ''}
    exit 0
  '';
in {
  options.modules.programs.crush = {
    enable = mkEnableOption "crush, the Charm Bracelet AI assistant";

    package = mkOption {
      type = types.package;
      default = pkgs.crush;
      description = "The crush package to install.";
    };

    anthropicApiKeySecret = mkOption {
      type = types.nullOr types.str;
      default = "anthropic-api-key";
      description = ''
        Name of the sops secret containing the Anthropic API key. Must be
        declared in `modules.programs.sops.secrets.<name>`. Set to `null` to
        skip sops integration entirely (e.g. when relying on a shell env var
        for `$ANTHROPIC_API_KEY`).
      '';
    };

    openrouterApiKeySecret = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "openrouter-crush-api-key";
      description = ''
        Name of the sops secret containing the OpenRouter API key. Must be
        declared in `modules.programs.sops.secrets.<name>`. Defaults to `null`
        (opt-in); set it to wire OpenRouter's `api_key` from sops. Remember to
        also add `openrouter` to `providers` and reference it from `models`.
      '';
    };

    providers = mkOption {
      type = types.attrsOf types.attrs;
      default = {anthropic = {};};
      description = ''
        Provider configurations keyed by provider id. The built-in
        `anthropic` provider only needs an `api_key`; other fields (base_url,
        models metadata) come from crush's embedded provider catalog.
      '';
    };

    models = mkOption {
      type = types.attrsOf types.attrs;
      default = {
        large = {
          provider = "anthropic";
          model = "claude-opus-4-7";
          think = true;
        };
        small = {
          provider = "anthropic";
          model = "claude-haiku-4-5-20251001";
        };
      };
      description = ''
        Model selections, typically `large` and `small`. These are the model
        slots crush picks from for different operations (`large` for
        reasoning-heavy work, `small` for cheap summarization).
      '';
    };

    reuseClaudeConfig = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Drive crush from the Claude Code configuration in this repo:

        - `config/claude/CLAUDE.md` is installed as `~/.config/crush/CRUSH.md`,
          one of crush's default global context paths.
        - `config/claude/skills` becomes crush's global skills directory on
          hosts where the claude module isn't already providing
          `~/.claude/skills`, which crush scans natively.
        - The `Bash(...)` rules in `modules.programs.claude.allowedTools` and
          `deniedTools` are enforced through a PreToolUse hook on crush's
          `bash` tool, so the one curated command allowlist serves both.

        Everything else crush should know about — `mcp`, `lsp`, more hooks,
        `tui`, further providers — goes straight into
        `home.programs.crush.settings`; home-manager deep-merges it with what
        this module derives.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions =
      mapAttrsToList (providerId: secretName: {
        assertion = config.sops.secrets ? ${secretName};
        message = ''
          modules.programs.crush.${providerId}ApiKeySecret is set to "${secretName}"
          but no matching secret is declared in modules.programs.sops.secrets.
        '';
      })
      apiKeySecrets;

    home.programs.crush = {
      enable = true;
      inherit (cfg) package;

      # Servers declared in programs.mcp.servers reach both crush and
      # claude-code, which sets the same flag.
      enableMcpIntegration = true;

      settings = mkMerge [
        {
          inherit providers;
          inherit (cfg) models;

          # Same stance as programs.claude-code.settings.includeCoAuthoredBy.
          options.attribution = {
            trailer_style = mkDefault "none";
            generated_with = mkDefault false;
          };
        }
        (mkIf cfg.reuseClaudeConfig {
          # `Read(*)`: crush's grep/glob never prompt, and view/ls only do for
          # paths outside the working directory.
          permissions.allowed_tools = ["view" "ls"];

          hooks.PreToolUse = [
            {
              name = "claude allowlist";
              matcher = "^bash$";
              command = "${bashPermissionHook}";
              timeout = 5;
            }
          ];
        })
      ];

      # Crush already scans ~/.claude/skills as a global skills directory, so
      # while the claude module installs that tree a second copy under
      # ~/.config/crush/skills is redundant (crush dedups by name, last
      # scanned wins).
      skills =
        mkIf (cfg.reuseClaudeConfig && !claude.enable)
        ../../../../config/claude/skills;
    };

    home.configFile = mkIf cfg.reuseClaudeConfig {
      "crush/CRUSH.md".source = ../../../../config/claude/CLAUDE.md;
    };
  };
}
