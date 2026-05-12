{
  config,
  lib,
  pkgs,
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
        "Read(*)"
        "WebFetch(domain:crates.io)"
        "WebFetch(domain:docs.rs)"
        "WebFetch(domain:github.com)"
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
    user.programs = with pkgs; [
      # some of the plugins below use python3 and assume it's globally available, which of course it isn't
      python3
    ];

    home.programs.claude-code = {
      enable = true;
      enableMcpIntegration = true;

      settings = {
        model = "opus";
        alwaysThinkingEnabled = true;
        effortLevel = "high";

        permissions = {
          allow = cfg.allowedTools;
          deny = cfg.deniedTools;
        };

        enabledPlugins = {
          "feature-dev@claude-plugins-official" = true;
          "code-review@claude-plugins-official" = true;
          "explanatory-output-style@claude-plugins-official" = true;
          "frontend-design@claude-plugins-official" = true;
          "security-guidance@claude-plugins-official" = true;
          "typescript-lsp@claude-plugins-official" = true;
          "rust-analyzer-lsp@claude-plugins-official" = true;
          "code-review-ai@claude-code-workflows" = true;
          "systems-programming@claude-code-workflows" = true;
          "agent-orchestration@claude-code-workflows" = true;
          "api-scaffolding@claude-code-workflows" = true;
          "api-testing-observability@claude-code-workflows" = true;
          "tdd-workflows@claude-code-workflows" = true;
          "backend-api-security@claude-code-workflows" = true;
          "backend-development@claude-code-workflows" = true;
          "code-documentation@claude-code-workflows" = true;
          "code-refactoring@claude-code-workflows" = true;
          "codebase-cleanup@claude-code-workflows" = true;
          "data-engineering@claude-code-workflows" = true;
          "database-design@claude-code-workflows" = true;
          "database-migrations@claude-code-workflows" = true;
          "debugging-toolkit@claude-code-workflows" = true;
          "dependency-management@claude-code-workflows" = true;
          "deployment-strategies@claude-code-workflows" = true;
          "documentation-generation@claude-code-workflows" = true;
          "error-debugging@claude-code-workflows" = true;
          "error-diagnostics@claude-code-workflows" = true;
          "functional-programming@claude-code-workflows" = true;
        };
      };

      skills = "${config.dotfiles.configDir}/claude/skills";
      context = "${config.dotfiles.configDir}/claude/CLAUDE.md";
    };
  };
}
