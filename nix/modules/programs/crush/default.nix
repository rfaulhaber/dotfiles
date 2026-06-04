{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.crush;

  # Map of crush provider id -> sops secret name, restricted to providers
  # that actually have a secret wired up. When this is empty we skip sops
  # entirely and render a plain crush.json.
  apiKeySecrets =
    filterAttrs (_: secret: secret != null) {
      anthropic = cfg.anthropicApiKeySecret;
      openrouter = cfg.openrouterApiKeySecret;
    };

  hasAnySecret = apiKeySecrets != {};

  baseConfig =
    {
      "$schema" = "https://charm.land/crush.json";
      inherit (cfg) providers models;
      options = cfg.crushOptions;
      permissions = cfg.permissions;
    }
    // cfg.extraConfig;

  # Inject the sops placeholder into providers.<id>.api_key for every
  # provider that has a secret. The placeholder is substituted by sops-nix
  # at activation time when rendering the template. `(... or {})` ensures the
  # provider entry exists even if the user didn't pre-declare it in
  # `providers`.
  withApiKeys =
    baseConfig
    // {
      providers =
        foldl' (
          providers: providerId:
            providers
            // {
              ${providerId} =
                (providers.${providerId} or {})
                // {
                  api_key = config.sops.placeholder.${apiKeySecrets.${providerId}};
                };
            }
        )
        baseConfig.providers
        (attrNames apiKeySecrets);
    };

  outerConfig = config;
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

    crushOptions = mkOption {
      type = types.attrs;
      default = {};
      description = ''
        The crush `options` block (top-level UI/runtime knobs:
        `context_paths`, `disabled_tools`, `attribution`, etc.).
      '';
    };

    permissions = mkOption {
      type = types.attrs;
      default = {};
      description = "The crush `permissions` block (`allowed_tools` list).";
    };

    extraConfig = mkOption {
      type = types.attrs;
      default = {};
      description = ''
        Extra top-level keys merged into crush.json (e.g. `mcp`, `lsp`,
        `hooks`). Anything here overrides built-in keys.
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

    user.packages = [cfg.package];

    sops.templates = mkIf hasAnySecret {
      "crush.json" = {
        content = builtins.toJSON withApiKeys;
        owner = config.user.name;
        group = config.user.group;
        mode = "0400";
      };
    };

    home-manager.users.${config.user.name} = {config, ...}: {
      xdg.configFile."crush/crush.json".source =
        if hasAnySecret
        then
          config.lib.file.mkOutOfStoreSymlink
          outerConfig.sops.templates."crush.json".path
        else pkgs.writeText "crush.json" (builtins.toJSON baseConfig);
    };
  };
}
