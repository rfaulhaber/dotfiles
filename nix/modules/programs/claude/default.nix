{
  config,
  lib,
  pkgs,
  inputs,
  isLinux,
  ...
}:
with lib; let
  cfg = config.modules.programs.claude;
  mcp = config.modules.programs.mcp;
in {
  options.modules.programs.claude = {
    enable = mkEnableOption false;

    allowedTools = mkOption {
      type = types.listOf types.str;
      default = [
        # Deliberately absent, despite being the two highest-frequency prompts:
        # `ssh` and `ssh-bash` (a blanket allow authorizes any remote command,
        # including deploy-rs and nixos-rebuild switch) and `python3`
        # (arbitrary code). The prompt is the only thing gating those.

        # Rust
        "Bash(cargo *)"

        # Nix — evaluation, build, and query only. Nothing that activates a
        # system generation, and nothing that mutates the store.
        "Bash(nix eval*)"
        "Bash(nix build*)"
        "Bash(nix develop*)"
        "Bash(nix fmt*)"
        "Bash(nix flake check*)"
        "Bash(nix flake metadata*)"
        "Bash(nix flake show*)"
        "Bash(nix why-depends*)"
        "Bash(nix path-info*)"
        "Bash(nix derivation show*)"
        "Bash(nix-instantiate*)"
        "Bash(nix-store -q*)"

        # Git. `git add` only stages, and flake evaluation cannot see untracked
        # files, so it is a prerequisite for most nix work here rather than a
        # write. Note these are prefix globs: `git -C <dir> status` matches none
        # of them and still prompts.
        "Bash(git status*)"
        "Bash(git log*)"
        "Bash(git diff*)"
        "Bash(git branch*)"
        "Bash(git show*)"
        "Bash(git rev-parse*)"
        "Bash(git ls-files*)"
        "Bash(git add*)"

        # Read-only commands
        "Bash(find *)"
        "Bash(grep *)"
        "Bash(rg *)"
        "Bash(sed -n*)"
        "Bash(wc *)"
        "Bash(ls *)"
        "Bash(man *)"
        "Bash(which *)"
        "Bash(journalctl*)"
        "Bash(systemctl status*)"
        "Bash(systemctl cat*)"
        "Bash(nix run nixpkgs#ripgrep *)"
        "Bash(nix run nixpkgs#jq *)"
        "Bash(nix run nixpkgs#fd *)"
        "Bash(nix run nixpkgs#tree *)"
        "Bash(nix run nixpkgs#bat *)"
        "Bash(nix run nixpkgs#yq *)"
        "Bash(nix run nixpkgs#eza *)"
        "Bash(nix run nixpkgs#dasel *)"
        "Bash(nix run nixpkgs#gron *)"
        "Bash(nix run nixpkgs#glow *)"
        "Bash(nix run nixpkgs#htop *)"
        "Bash(nix run nixpkgs#btop *)"
        "Read(*)"
        "WebFetch(domain:crates.io)"
        "WebFetch(domain:docs.rs)"
        "WebFetch(domain:github.com)"
        "WebFetch(domain:api.github.com)"
        "WebFetch(domain:raw.githubusercontent.com)"
        "WebFetch(domain:nixos.wiki)"
        "WebFetch(domain:search.nixos.org)"
        "WebSearch"
      ];
      description = ''
        Tool patterns to allow without prompting.
        Uses glob syntax: Bash(command *) matches any bash call starting with "command".
        Setting this in a host config replaces the defaults. To extend them, use:
          modules.programs.claude.allowedTools = lib.mkAfter [ "Bash(npm *)" ];
        The `Bash(...)` entries also drive crush's bash permission hook and
        opencode's `permission.bash` rules when the respective
        `reuseClaudeConfig` option is on.
      '';
    };

    deniedTools = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Tool patterns to always deny.";
    };
  };

  config = let
    # Every field here is optional and its shape varies between releases, so
    # each accessor tolerates the key being absent, null, or the wrong type —
    # a statusline that errors renders as a bare error string on every frame.
    # rate_limits only appears on subscription auth, and only after the first
    # API response of the session.
    statusLineProgram = pkgs.writeText "claude-statusline.jq" ''
      def sstr: select(type == "string" and . != "");
      def tilde:
        (env.HOME // "") as $h
        | if ($h != "" and startswith($h)) then "~" + .[($h | length):] else . end;

      def model_name: if (.model | type) == "object" then .model.display_name else .model end;
      def style_name: if (.output_style | type) == "object" then .output_style.name else .output_style end;
      def work_dir:   (if (.workspace | type) == "object" then .workspace.current_dir else null end) // .cwd;
      def cost_usd:
        if (.cost | type) == "object" then .cost.total_cost_usd
        elif (.session | type) == "object" then .session.total_cost_usd
        else null end;
      def effort_level: if (.effort | type) == "object" then .effort.level else null end;
      def context_pct:
        if (.context_window | type) == "object" then .context_window.used_percentage else null end;
      def session_pct:
        if (.rate_limits | type) == "object" and (.rate_limits.five_hour | type) == "object"
        then .rate_limits.five_hour.used_percentage
        else null end;

      [ "[" + ((model_name | sstr) // "?") + "]"
      , (work_dir | sstr | tilde)
      , (style_name | sstr | select(. != "default"))
      , (effort_level | sstr)
      , (cost_usd | select(type == "number" and . > 0) | "$" + (. * 100 | round / 100 | tostring))
      , (context_pct | select(type == "number") | "ctx " + (round | tostring) + "%")
      , (session_pct | select(type == "number") | "5h " + (round | tostring) + "%")
      , (select(.exceeds_200k_tokens == true) | "⚠ 200k+")
      ]
      | map(select(. != null and . != ""))
      | join(" · ")
    '';

    statusLine = pkgs.writeShellScript "claude-statusline" ''
      exec ${pkgs.jq}/bin/jq -rf ${statusLineProgram}
    '';

    # Formats .nix files as they are written, so `nix fmt` stops being a manual
    # step. Every failure path exits 0: a hook that fails would surface as a
    # tool error, and mid-edit files that don't parse yet are the common case,
    # not an exception worth reporting.
    nixFmtOnWrite = pkgs.writeShellScript "claude-nix-fmt-on-write" ''
      set -u
      file=$(${pkgs.jq}/bin/jq -r '.tool_input.file_path // empty')
      case "$file" in
        *.nix) ;;
        *) exit 0 ;;
      esac
      [ -f "$file" ] || exit 0
      command -v nix >/dev/null 2>&1 || exit 0

      # `nix fmt` resolves the formatter from the enclosing flake, so it has to
      # run at the flake root; a repo without one has nothing to run.
      root=$(${pkgs.git}/bin/git -C "$(dirname "$file")" rev-parse --show-toplevel 2>/dev/null) || exit 0
      [ -n "$root" ] && [ -f "$root/flake.nix" ] || exit 0

      (cd "$root" && nix fmt "$file" >/dev/null 2>&1) || true
      exit 0
    '';

    # The Bash tool never runs direnv's shell hook, so in a project whose
    # devshell comes from .envrc every command needed a `nix develop -c`
    # wrapper unless claude happened to start inside the devshell.
    # SessionStart and CwdChanged hooks may write a script to $CLAUDE_ENV_FILE
    # that Claude sources before each Bash command; this fills it with
    # direnv's export for the session's directory. Like the formatter hook,
    # every failure path exits 0, and nothing may reach stdout, which
    # SessionStart would add to the model's context.
    direnvEnv = pkgs.writeShellScript "claude-direnv-env" ''
      set -u
      [ -n "''${CLAUDE_ENV_FILE-}" ] || exit 0
      command -v direnv >/dev/null 2>&1 || exit 0
      dir=$(${pkgs.jq}/bin/jq -r '.new_cwd // .cwd // empty')
      [ -n "$dir" ] && cd "$dir" 2>/dev/null || exit 0

      # direnv reports an allowed .envrc as status 0.
      allowed() {
        [ "$(direnv status --json 2>/dev/null | ${pkgs.jq}/bin/jq -r '.state.foundRC.allowed // empty')" = 0 ]
      }

      if ! allowed; then
        # A linked git worktree puts .envrc at a path direnv has never been
        # asked to trust, or lacks it entirely when it is untracked in the
        # main checkout. Fall back to the main checkout's trust decision.
        top=$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null) || exit 0
        common=$(${pkgs.git}/bin/git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || exit 0
        main=$(dirname "$common")
        [ "$main" != "$top" ] && [ -f "$main/.envrc" ] || exit 0
        (cd "$main" && allowed) || exit 0
        if [ -f "$top/.envrc" ]; then
          # Trust extends only to a byte-identical copy of the trusted file;
          # a branch that edited .envrc still needs a human `direnv allow`.
          ${pkgs.diffutils}/bin/cmp -s "$main/.envrc" "$top/.envrc" || exit 0
          direnv allow "$top" >/dev/null 2>&1 || exit 0
        else
          cd "$main" || exit 0
        fi
      fi

      direnv export bash >"$CLAUDE_ENV_FILE" 2>/dev/null || true
      exit 0
    '';

    # home-manager renders mcpServers into a plugin whose .mcp.json is a
    # world-readable store path, so a secret can never be written into it.
    # Claude Code expands `${VAR}` in MCP headers from its own environment,
    # so a header names a variable and this launcher fills it from the sops
    # path at exec time. An already-exported variable wins, so a one-off
    # token can still be tested without a rebuild.
    sanitize = s:
      stringAsChars (
        c:
          if builtins.match "[A-Za-z0-9]" c == null
          then "_"
          else c
      ) (toUpper s);
    secretVar = {
      server,
      header,
      ...
    }: "MCP_${sanitize server}_${sanitize header}";

    secretHeaders = mcp.lib.secretHeadersFor "claude";

    launcherFor = {
      server,
      path,
      ...
    } @ entry: let
      var = secretVar entry;
    in ''
      if [ -z "''${${var}-}" ]; then
        if [ -r ${escapeShellArg path} ]; then
          export ${var}="$(<${escapeShellArg path})"
        else
          echo "claude: ${path} is unreadable; the ${server} MCP server will fail to authenticate" >&2
        fi
      fi
    '';

    # /tmp is a RAM-backed tmpfs on every NixOS host, and Claude Code keeps
    # task output, scratchpads and plugin staging under its temp root; a few
    # long sessions fill it and output is lost to ENOSPC. The binary appends
    # claude-<uid> to this directory and refuses a root it does not own. The
    # tmpfiles rule below ages out what sessions leave behind.
    tmpDirSetup = optionalString isLinux ''
      export CLAUDE_CODE_TMPDIR="''${CLAUDE_CODE_TMPDIR:-''${XDG_CACHE_HOME:-$HOME/.cache}/claude-code/tmp}"
      mkdir -p -m 0700 "$CLAUDE_CODE_TMPDIR"
    '';

    launcherSetup = tmpDirSetup + concatMapStrings launcherFor secretHeaders;

    claudePackage =
      if launcherSetup == ""
      then pkgs.claude-code
      else
        pkgs.symlinkJoin {
          name = "claude-code-wrapped";
          paths = [pkgs.claude-code];
          # home-manager gates plugin loading on the package version; a
          # wrapper without it falls back to the legacy --plugin-dir mode.
          inherit (pkgs.claude-code) version meta;
          nativeBuildInputs = [pkgs.makeWrapper];
          postBuild = ''
            wrapProgram $out/bin/claude --run ${escapeShellArg launcherSetup}
          '';
        };

    # Every host's login shell is nushell, so `ssh host '<cmd>'` hands <cmd>
    # to nushell's parser, where regex escapes, `&&` and `find -maxdepth`
    # all break. A script on stdin reaches bash untouched and needs no
    # second layer of quoting. Without a heredoc, stdin is whatever the caller
    # inherited: a terminal, or under Claude Code's Bash tool a socket that
    # never closes, which would leave `bash -s` waiting forever.
    sshBash = pkgs.writeShellScriptBin "ssh-bash" ''
      if [ $# -lt 1 ] || [ -t 0 ] || [ -S /dev/stdin ]; then
        echo "usage: ssh-bash [ssh-options] <host> <<'EOF' ... EOF" >&2
        exit 2
      fi
      exec ssh "$@" bash -s
    '';

    mcpServers =
      mapAttrs (
        _: server:
          if server.command != null
          then
            {
              type = "stdio";
              inherit (server) command;
            }
            // optionalAttrs (server.args != []) {inherit (server) args;}
          else
            {
              type = "http";
              inherit (server) url;
            }
            // optionalAttrs (server.headers != {}) {inherit (server) headers;}
      )
      (mcp.lib.serversFor "claude" (entry: "\${${secretVar entry}}"));
  in
    mkIf cfg.enable (mkMerge [
      {
        # Replaces pkgs.claude-code (and thus the home-manager module's default
        # package) with the always-current build from the claude-code-nix flake.
        nixpkgs.overlays = [inputs.claude-code.overlays.default];

        user.packages = with pkgs; [
          # some of the plugins below use python3 and assume it's globally available, which of course it isn't
          python3
          sshBash
        ];

        home.programs.claude-code = {
          enable = true;
          package = claudePackage;

          settings = {
            includeCoAuthoredBy = false;
            tui = "fullscreen";
            remoteControlAtStartup = false;

            # Transcripts are the only record of tool calls, agents and cost;
            # the 30-day default deletes them before a bimonthly usage review
            # (the usage-review skill) can read them.
            cleanupPeriodDays = 120;

            permissions = {
              allow = cfg.allowedTools;
              deny = cfg.deniedTools;
            };

            statusLine = {
              type = "command";
              command = "${statusLine}";
            };

            hooks = {
              # Re-inject the nushell rule on every prompt. UserPromptSubmit stdout is
              # added to model context, which counters the salience decay of a rule
              # that's otherwise only loaded once from CLAUDE.md at session start.
              UserPromptSubmit = [
                {
                  hooks = [
                    {
                      type = "command";
                      command = "echo 'Reminder: any shell command you hand me to run goes in nushell syntax, not bash (the Bash tool you run yourself is exempt for single external invocations).'";
                    }
                  ];
                }
              ];

              # A cold nix-direnv cache (a fresh worktree) evaluates the flake,
              # which can outlast the default hook timeout.
              SessionStart = [
                {
                  hooks = [
                    {
                      type = "command";
                      command = "${direnvEnv}";
                      timeout = 120;
                    }
                  ];
                }
              ];
              CwdChanged = [
                {
                  hooks = [
                    {
                      type = "command";
                      command = "${direnvEnv}";
                      timeout = 120;
                    }
                  ];
                }
              ];

              PostToolUse = [
                {
                  matcher = "Edit|Write|MultiEdit";
                  hooks = [
                    {
                      type = "command";
                      command = "${nixFmtOnWrite}";
                    }
                  ];
                }
              ];
            };
          };

          # Marketplace plugins as Nix-pinned skills-dir plugins: home-manager
          # links each one into ~/.claude/skills/<name>, and Claude Code loads any
          # entry there carrying .claude-plugin/plugin.json as a plugin. Versions
          # follow flake.lock rather than Claude Code's runtime updater, so a bump
          # is `nix flake update <input>`. The names share a namespace with the
          # skills directory below and must stay unique across both.
          plugins = let
            # Marketplace repos keep each plugin under plugins/<name>.
            fromMarketplace = input: names:
              genAttrs names (name: "${input}/plugins/${name}");
          in
            # Every enabled plugin's skill and agent descriptions ride along in
            # every request, so a plugin earns its place by being invoked.
            fromMarketplace inputs.claude-plugins-official [
              "claude-code-setup"
              "code-review"
              "explanatory-output-style"
              "frontend-design"
              "ralph-loop"
              "security-guidance"
              "skill-creator"
            ]
            // {
              # Repositories that are a single plugin at their root.
              superpowers = "${inputs.superpowers}";
              agent-skills = "${inputs.addy-agent-skills}";
            };

          # The official marketplace's *-lsp plugins are README-only: their server
          # definitions sit in the marketplace entry, which a skills-dir plugin
          # never sees. The two that were enabled are reproduced here verbatim;
          # home-manager renders them into its generated plugin's .lsp.json.
          # This should also only contain global LSP servers. Projects should
          # define their own MCP servers.
          lspServers = {
            rust-analyzer = {
              command = "rust-analyzer";
              extensionToLanguage = {".rs" = "rust";};
            };
            typescript = {
              command = "typescript-language-server";
              args = ["--stdio"];
              extensionToLanguage = {
                ".ts" = "typescript";
                ".tsx" = "typescriptreact";
                ".js" = "javascript";
                ".jsx" = "javascriptreact";
                ".mts" = "typescript";
                ".cts" = "typescript";
                ".mjs" = "javascript";
                ".cjs" = "javascript";
              };
            };
          };

          # Projects should provide their own project-specific MCP servers;
          # this holds only the global ones. Tools land under
          # `mcp__plugin_hm_<server>__*` because home-manager ships them
          # through its generated plugin.
          inherit mcpServers;

          # Path literals, not dotfiles.configDir: home-manager copies these into a
          # sandboxed derivation, and a toString'd path carries no store context to
          # register as a build input.
          skills = ../../../../config/claude/skills;

          # Pinning the tier in each agent's frontmatter makes picking the agent
          # equivalent to picking the model, so cheap subagents stop depending on
          # the main loop remembering to pass `model:`.
          agentsDir = ../../../../config/claude/agents;

          context = ../../../../config/claude/CLAUDE.md;
        };

        # Saved Workflow scripts, run by name with `args`. home-manager's
        # claude-code module has no option for this directory.
        home.file.".claude/workflows".source = ../../../../config/claude/workflows;
      }

      (optionalAttrs isLinux {
        systemd.user.tmpfiles.users.${config.user.name}.rules = [
          "d %C/claude-code/tmp 0700 - - 7d"
        ];
      })
    ]);
}
