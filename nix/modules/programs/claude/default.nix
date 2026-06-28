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
        # Rust
        "Bash(cargo *)"

        # Git (read-only)
        "Bash(git status*)"
        "Bash(git log*)"
        "Bash(git diff*)"
        "Bash(git branch*)"
        "Bash(git show*)"
        "Bash(git rev-parse*)"
        "Bash(git ls-files*)"

        # Read-only commands
        "Bash(find *)"
        "Bash(grep *)"
        "Bash(ls *)"
        "Bash(man *)"
        "Bash(which *)"
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

  config = mkIf cfg.enable {
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
        model = "claude-opus-4-8";
        alwaysThinkingEnabled = true;
        effortLevel = "high";

        permissions = {
          allow = cfg.allowedTools;
          deny = cfg.deniedTools;
        };

        # Re-inject the nushell rule on every prompt. UserPromptSubmit stdout is
        # added to model context, which counters the salience decay of a rule
        # that's otherwise only loaded once from CLAUDE.md at session start.
        hooks = {
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
        };

        enabledPlugins = let
          plugins = [
            "agent-orchestration@claude-code-workflows"
            "api-scaffolding@claude-code-workflows"
            "api-testing-observability@claude-code-workflows"
            "backend-api-security@claude-code-workflows"
            "backend-development@claude-code-workflows"
            "claude-code-setup@claude-plugins-official"
            "claude-md-management@claude-plugins-official"
            "code-documentation@claude-code-workflows"
            "code-refactoring@claude-code-workflows"
            "code-review@claude-plugins-official"
            "code-simplifier@claude-plugins-official"
            "codebase-cleanup@claude-code-workflows"
            "data-engineering@claude-code-workflows"
            "database-design@claude-code-workflows"
            "database-migrations@claude-code-workflows"
            "debugging-toolkit@claude-code-workflows"
            "dependency-management@claude-code-workflows"
            "deployment-strategies@claude-code-workflows"
            "documentation-generation@claude-code-workflows"
            "error-debugging@claude-code-workflows"
            "error-diagnostics@claude-code-workflows"
            "explanatory-output-style@claude-plugins-official"
            "feature-dev@claude-plugins-official"
            "frontend-design@claude-plugins-official"
            "functional-programming@claude-code-workflows"
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
      context = ../../../../config/claude/CLAUDE.md;
    };
  };
}
