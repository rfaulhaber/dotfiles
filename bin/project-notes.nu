#!/usr/bin/env nu

# Replaces a repository's untracked `notes` directory with a symlink into a
# directory synced between machines, one subdirectory per repository. Keying
# on the repository name rather than the checkout path means one repository
# cloned at different paths, or checked out as several worktrees, shares the
# same notes everywhere. The symlink is untracked, so this runs once per
# checkout on each machine. The root comes from $env.PROJECT_NOTES_ROOT unless
# --root is given.

def --wrapped git-rev-parse [...args: string]: nothing -> string {
    let result = ^git rev-parse ...$args | complete
    if $result.exit_code != 0 {
        error make {msg: "not inside a git repository"}
    }
    $result.stdout | str trim
}

# Named after the main checkout so every worktree of a repository resolves to
# the same notes directory.
def main-checkout []: nothing -> string {
    let common = git-rev-parse --path-format=absolute --git-common-dir
    if ($common | path basename) == ".git" {
        $common | path dirname
    } else {
        $common
    }
}

# Link the current checkout's notes directory into the synced root, moving
# any existing local notes into it first.
def main [
    name?: string   # Directory under the root; defaults to the main checkout's directory name
    --root: string  # Synced notes root; defaults to $env.PROJECT_NOTES_ROOT
]: nothing -> record {
    let root = $root | default $env.PROJECT_NOTES_ROOT?
    if $root == null {
        error make {msg: "no notes root: set PROJECT_NOTES_ROOT or pass --root"}
    }

    let checkout = git-rev-parse --show-toplevel
    let name = $name | default (main-checkout | path basename)
    let target = $root | path join $name | path expand --no-symlink
    let link = $checkout | path join notes
    mkdir $target

    let link_type = $link | path type | default ""
    let previous = if $link_type == "symlink" {
        ls --long --directory $link | get 0.target
    }
    if $previous != null and ($previous | path expand --no-symlink) != $target {
        error make {msg: $"($link) already links to ($previous); remove it to relink"}
    }
    if $link_type not-in ["", "symlink", "dir"] {
        error make {msg: $"($link) is a ($link_type), not a notes directory"}
    }

    # Local notes move into the synced directory one entry at a time. A name
    # already present there came from another machine, and overwriting it
    # would lose that machine's copy, so it stays behind in the local
    # directory for a hand merge and the link waits until that is resolved.
    let local = if $link_type == "dir" { ls --all $link | get name } else { [] }
    let conflicts = $local | where {|f| $target | path join ($f | path basename) | path exists }
    let moved = $local | where {|f| $f not-in $conflicts }
    for f in $moved { mv $f $target }

    let linked = if $link_type == "symlink" {
        true
    } else if ($conflicts | is-empty) {
        if $link_type == "dir" { rm $link }
        ^ln -s $target $link
        true
    } else {
        false
    }

    # A `notes/` ignore pattern only matches directories, and git sees the
    # symlink as a file, so the checkout needs a pattern without the slash.
    let ignored = (^git -C $checkout check-ignore --quiet notes | complete).exit_code == 0
    let exclude_file = git-rev-parse --path-format=absolute --git-common-dir | path join info exclude
    if $linked and not $ignored {
        mkdir ($exclude_file | path dirname)
        "/notes\n" | save --append $exclude_file
    }

    {
        link: $link
        notes: $target
        linked: $linked
        moved: ($moved | length)
        conflicts: ($conflicts | each { path basename })
        excluded_in: (if $linked and not $ignored { $exclude_file })
    }
}
