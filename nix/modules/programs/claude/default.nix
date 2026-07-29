{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.programs.claude;
in {
  options.modules.programs.claude = {
    enable = mkEnableOption false;

    allowedTools = mkOption {
      type = types.listOf types.str;
      default = [
        # Deliberately absent, despite being the two highest-frequency prompts:
        # `ssh` (a blanket allow authorizes any remote command, including
        # deploy-rs and nixos-rebuild switch) and `python3` (arbitrary code).
        # The prompt is the only thing gating those.

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
    # Effort is not a documented part of the payload; the accessor is
    # speculative and simply contributes nothing when the key is missing.
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

      [ "[" + ((model_name | sstr) // "?") + "]"
      , (work_dir | sstr | tilde)
      , (style_name | sstr | select(. != "default"))
      , ((.effortLevel // .reasoning_effort) | sstr)
      , (cost_usd | select(type == "number" and . > 0) | "$" + (. * 100 | round / 100 | tostring))
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
  in
    mkIf cfg.enable {
      # Replaces pkgs.claude-code (and thus the home-manager module's default
      # package) with the always-current build from the claude-code-nix flake.
      nixpkgs.overlays = [inputs.claude-code.overlays.default];

      user.packages = with pkgs; [
        # some of the plugins below use python3 and assume it's globally available, which of course it isn't
        python3
      ];

      home.programs.claude-code = {
        enable = true;
        enableMcpIntegration = true;

        settings = {
          includeCoAuthoredBy = false;
          tui = "fullscreen";
          disableRemoteControl = true;
          remoteControlAtStartup = false;

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

          enabledPlugins = let
            plugins = [
              "agent-orchestration@claude-code-workflows"
              "api-testing-observability@claude-code-workflows"
              "backend-api-security@claude-code-workflows"
              "backend-development@claude-code-workflows"
              "claude-code-setup@claude-plugins-official"
              "claude-md-management@claude-plugins-official"
              "code-refactoring@claude-code-workflows"
              "code-review@claude-plugins-official"
              "code-simplifier@claude-plugins-official"
              "codebase-cleanup@claude-code-workflows"
              "database-design@claude-code-workflows"
              "database-migrations@claude-code-workflows"
              "debugging-toolkit@claude-code-workflows"
              "deployment-strategies@claude-code-workflows"
              "documentation-generation@claude-code-workflows"
              "error-debugging@claude-code-workflows"
              "explanatory-output-style@claude-plugins-official"
              "feature-dev@claude-plugins-official"
              "frontend-design@claude-plugins-official"
              "learning-output-style@claude-plugins-official"
              "ralph-loop@claude-plugins-official"
              "rust-analyzer-lsp@claude-plugins-official"
              "security-guidance@claude-plugins-official"
              "skill-creator@claude-plugins-official"
              "superpowers@claude-plugins-official"
              "systems-programming@claude-code-workflows"
              "tdd-workflows@claude-code-workflows"
              "typescript-lsp@claude-plugins-official"
            ];
          in
            builtins.foldl' (acc: el: {"${el}" = true;} // acc) {} plugins;
        };

        skills = "${config.dotfiles.configDir}/claude/skills";

        # Pinning the tier in each agent's frontmatter makes picking the agent
        # equivalent to picking the model, so cheap subagents stop depending on
        # the main loop remembering to pass `model:`.
        agentsDir = "${config.dotfiles.configDir}/claude/agents";

        context = ../../../../config/claude/CLAUDE.md;
      };
    };
}
