{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.programs.mcp;

  # Every agent module listed here must expose `enable`; it decides whether a
  # server opted in for that agent counts as active on the host.
  agentNames = ["claude" "crush" "opencode"];

  secretHeaderOpts = {
    options = {
      secret = mkOption {
        type = types.str;
        description = "Name of the sops secret holding the header value.";
        example = "github_mcp";
      };
      prefix = mkOption {
        type = types.str;
        default = "";
        description = "Text placed before the secret in the header value.";
        example = "Bearer ";
      };
    };
  };

  serverOpts = {
    options = {
      command = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Executable of a local server. Mutually exclusive with `url`.";
        example = "codegraph";
      };
      args = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Arguments passed to `command`.";
        example = ["serve" "--mcp"];
      };
      url = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Endpoint of a remote HTTP server. Mutually exclusive with `command`.";
        example = "https://example.com/mcp";
      };
      headers = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = ''
          Request headers, passed through unchanged, so each agent's own
          interpolation syntax still applies to the values: `''${VAR}` for
          Claude, `$VAR` and `$(...)` for crush, `{env:...}` and `{file:...}`
          for opencode. Only valid with `url`.
        '';
      };
      secretHeaders = mkOption {
        type = types.attrsOf (types.submodule secretHeaderOpts);
        default = {};
        description = ''
          Request headers whose value is read from a sops secret. Each agent
          decides how to refer to the secret (see `lib.serversFor`), so the
          value never enters the store. Only valid with `url`.
        '';
      };
      # A fixed set of options rather than an attrset of booleans, so a typo
      # such as `agents.claud` fails evaluation instead of doing nothing.
      agents = genAttrs agentNames (agent: mkEnableOption "exposing this server to ${agent}");
    };
  };

  optedIn = agent: filterAttrs (_: server: server.agents.${agent}) cfg.servers;

  secretPath = secret: config.sops.secrets.${secret}.path;

  secretHeadersFor = agent:
    concatLists (mapAttrsToList (
      server: def:
        mapAttrsToList (header: {
          secret,
          prefix,
          ...
        }: {
          inherit server header secret prefix;
          path = secretPath secret;
        })
        def.secretHeaders
    ) (optedIn agent));

  serversFor = agent: formatSecret:
    mapAttrs (
      server: def:
        def
        // {
          headers =
            def.headers
            // mapAttrs (header: {
              secret,
              prefix,
              ...
            }:
              prefix
              + formatSecret {
                inherit server header;
                path = secretPath secret;
              })
            def.secretHeaders;
        }
    ) (optedIn agent);

  # Secrets are only required where an enabled agent would read them, so a
  # server can be defined centrally without every host declaring its tokens.
  isActive = server: any (agent: server.agents.${agent} && config.modules.programs.${agent}.enable) agentNames;

  serverAssertions = name: server: let
    where = "modules.programs.mcp.servers.${name}";
    lowerKeys = mapAttrs' (name: nameValuePair (toLower name));
    duplicateHeaders = attrNames (intersectAttrs (lowerKeys server.headers) (lowerKeys server.secretHeaders));
  in
    [
      {
        assertion = (server.command != null) != (server.url != null);
        message = "${where}: exactly one of `command` or `url` must be set.";
      }
      {
        assertion = server.args == [] || server.command != null;
        message = "${where}: `args` is only valid together with `command`.";
      }
      {
        assertion = (server.headers == {} && server.secretHeaders == {}) || server.url != null;
        message = "${where}: `headers` and `secretHeaders` are only valid together with `url`.";
      }
      {
        assertion = duplicateHeaders == [];
        message = "${where}: `headers` and `secretHeaders` both set ${concatMapStringsSep ", " (header: "`${header}`") duplicateHeaders}.";
      }
    ]
    ++ optionals (isActive server) (concatLists (mapAttrsToList (
        header: {secret, ...}: let
          # null unless the secret sets it, which is the likeliest way to get
          # the ownership wrong.
          owner = config.sops.secrets.${secret}.owner;
        in [
          {
            assertion = config.sops.secrets ? ${secret};
            message = ''
              ${where}: secretHeaders.${header}.secret is set to "${secret}"
              but no matching secret is declared in modules.programs.sops.secrets.
            '';
          }
          {
            # Vacuous when the secret is missing; the assertion above reports it.
            assertion = !(config.sops.secrets ? ${secret}) || owner == config.user.name;
            message = ''
              ${where}: secret "${secret}" (secretHeaders.${header}) must be owned by ${config.user.name},
              the user the agents run as, but its owner is ${
                if owner == null
                then "unset (the file is owned by `uid`, which defaults to root)"
                else ''"${owner}"''
              }.
            '';
          }
        ]
      )
      server.secretHeaders));
in {
  options.modules.programs.mcp = {
    servers = mkOption {
      type = types.attrsOf (types.submodule serverOpts);
      default = {};
      description = ''
        MCP servers shared by the coding agents. A server does nothing until
        an agent is opted into it with `agents.<agent>`; each agent module
        renders the servers it was given in its own config format.

        Unrelated to home-manager's `programs.mcp`.
      '';
    };

    lib = mkOption {
      type = types.attrs;
      internal = true;
      description = "Helper functions for the agent modules that render these servers.";
    };
  };

  config = {
    modules.programs.mcp = {
      # Neither helper checks whether the agent is enabled; callers do.
      lib = {
        # { server, header, secret, prefix, path } for every secret header of
        # a server opted in for `agent`.
        inherit secretHeadersFor;

        # The servers opted in for `agent`, with each secret header merged
        # into `headers` as `prefix + formatSecret { server, header, path }`.
        # Only the agent knows how it refers to a secret file, hence the
        # callback. All other fields are returned as defined.
        inherit serversFor;
      };

      servers = {
        codegraph = {
          command = "codegraph";
          args = ["serve" "--mcp"];
        };
        ebay.url = "https://ebay-mcp.3679.space/mcp";
        github = {
          url = "https://api.githubcopilot.com/mcp";
          secretHeaders.Authorization = {
            secret = "github_mcp";
            prefix = "Bearer ";
          };
        };
      };
    };

    assertions = concatLists (mapAttrsToList serverAssertions cfg.servers);
  };
}
