#!/usr/bin/env nu

# Opts a repository into Claude Code auto memory kept under a directory synced
# between machines. Claude Code's default memory directory is named after the
# checkout's absolute path, so one repository cloned at different paths gets
# unrelated memory on each machine. This points the checkout's
# `autoMemoryDirectory` at <root>/<repo> instead. The setting lives in
# `.claude/settings.local.json`, which is per-checkout and untracked, so this
# runs once per repository on each machine. The root comes from
# $env.CLAUDE_MEMORY_ROOT unless --root is given.

def --wrapped git-rev-parse [...args: string]: nothing -> string {
    let result = ^git rev-parse ...$args | complete
    if $result.exit_code != 0 {
        error make {msg: "not inside a git repository"}
    }
    $result.stdout | str trim
}

# Claude Code keys its default memory directory on the main checkout, so every
# worktree of a repository resolves to the same name and the same seed source.
def main-checkout []: nothing -> string {
    let common = git-rev-parse --path-format=absolute --git-common-dir
    if ($common | path basename) == ".git" {
        $common | path dirname
    } else {
        $common
    }
}

# Point the current checkout at synced memory, seeding it from the default
# memory directory when the synced one is still empty.
def main [
    name?: string   # Directory under the root; defaults to the main checkout's directory name
    --root: string  # Synced memory root; defaults to $env.CLAUDE_MEMORY_ROOT
]: nothing -> record {
    let root = $root | default $env.CLAUDE_MEMORY_ROOT?
    if $root == null {
        error make {msg: "no memory root: set CLAUDE_MEMORY_ROOT or pass --root"}
    }

    let checkout = git-rev-parse --show-toplevel
    let main = main-checkout
    let name = $name | default ($main | path basename)
    # Kept in its `~/` form: Claude Code expands it, and the file then reads
    # the same on every machine.
    let dir = $"($root | str trim --right --char '/')/($name)"

    let settings_file = $checkout | path join .claude settings.local.json
    mkdir ($settings_file | path dirname)
    # The file also holds the permission rules Claude Code saves from prompts,
    # so it is merged into, never replaced.
    let settings = if ($settings_file | path exists) { open $settings_file } else { {} }
    $settings | upsert autoMemoryDirectory $dir | save --force $settings_file

    let config_dir = $env.CLAUDE_CONFIG_DIR? | default "~/.claude"
    let slug = $main | str replace --all --regex '[^a-zA-Z0-9]' '-'
    let source = $config_dir | path join projects $slug memory | path expand
    let target = $dir | path expand

    let source_files = if ($source | path exists) { ls $source | get name } else { [] }
    let target_empty = not ($target | path exists) or (ls --all $target | is-empty)
    mkdir $target

    # A target with content was seeded already, here or on another machine;
    # copying over it would clobber MEMORY.md, so anything missing is only
    # reported for a hand merge.
    let seeded = if $target_empty and ($source_files | is-not-empty) {
        cp --recursive ...$source_files $target
        $source_files | length
    } else {
        0
    }
    let unmerged = if $target_empty {
        []
    } else {
        $source_files
            | where {|f| not ($target | path join ($f | path basename) | path exists) }
            | each { path basename }
    }

    {
        settings: $settings_file
        memory: $dir
        previous: $settings.autoMemoryDirectory?
        seeded_from: (if $seeded > 0 { $source })
        seeded: $seeded
        unmerged: $unmerged
    }
}
