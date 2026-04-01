#!/usr/bin/env nu

# Portable install script for non-Nix hosts.
# Symlinks dotfiles repo and generated configs into ~/.config.

def main [
    --dry-run # Show what would be done without making changes
] {
    let repo_dir = ($env.FILE_PWD)
    let config_dir = ($env.XDG_CONFIG_HOME? | default ($env.HOME | path join ".config"))
    let platform = (detect-platform)

    print $"Installing dotfiles \(platform: ($platform))"
    if $dry_run { print "(dry run)" }
    print ""

    # 1. Link repo -> ~/.config/dotfiles
    print "Linking dotfiles repo:"
    make-link $repo_dir ($config_dir | path join "dotfiles") $dry_run
    print ""

    # 2. Link generated platform configs -> ~/.config/
    let platform_dir = ($repo_dir | path join "generated" $platform)
    if ($platform_dir | path exists) {
        print $"Linking generated configs \(($platform)):"
        link-generated $platform_dir $config_dir $dry_run
        print ""
    } else {
        print $"  [WARN] No generated configs found at ($platform_dir)"
        print "  [WARN] Run 'just generate' on a Nix-enabled machine first."
        print ""
    }

    # 3. Link host-specific overrides -> ~/.config/ (highest priority)
    let hostname = (sys host | get hostname)
    let override_dir = ($repo_dir | path join "generated" "overrides" $hostname)
    if ($override_dir | path exists) {
        print $"Linking host overrides \(($hostname)):"
        link-generated $override_dir $config_dir $dry_run
        print ""
    }

    print "Done."
}

def detect-platform [] {
    match $nu.os-info.name {
        "linux" => "linux"
        "macos" => "darwin"
        _ => { error make { msg: $"Unsupported platform: ($nu.os-info.name)" } }
    }
}

def make-link [source: path, target: path, dry_run: bool] {
    if ($target | path type) == "symlink" {
        let existing = (ls -l $target | get 0.target)
        if $existing == ($source | path expand) {
            return
        }
        if $dry_run {
            print $"  Would relink ($target) -> ($source) \(was ($existing))"
        } else {
            rm $target
        }
    } else if ($target | path exists) {
        let backup = $"($target).bak"
        if $dry_run {
            print $"  Would back up ($target) -> ($backup)"
        } else {
            mv $target $backup
            print $"  Backed up ($target) -> ($backup)"
        }
    }

    let parent = ($target | path dirname)
    if not ($parent | path exists) {
        if $dry_run {
            print $"  Would create directory ($parent)"
        } else {
            mkdir $parent
        }
    }

    if $dry_run {
        print $"  Would link ($target) -> ($source)"
    } else {
        ln -s $source $target
        print $"  Linked ($target) -> ($source)"
    }
}

def link-generated [gen_dir: path, config_dir: path, dry_run: bool] {
    glob ($gen_dir | path join "**" "*")
    | where ($it | path type) == "file"
    | each { |file|
        let rel = ($file | str replace $"($gen_dir)/" "")
        make-link $file ($config_dir | path join $rel) $dry_run
    }
    null
}
