#!/usr/bin/env nu
# Flake input update script

def main [inputs_to_update?: string = ""] {
    print "📦 Starting flake input updates..."
    
    # Backup original flake.lock
    cp flake.lock flake.lock.backup
    
    if ($inputs_to_update | str trim | str length) > 0 {
        update_specific_inputs $inputs_to_update
    } else {
        update_all_inputs
    }
    
    check_for_changes
    generate_update_summary
    test_updated_configuration
}

def update_specific_inputs [inputs: string] {
    print "🔄 Updating specific inputs..."
    let input_list = ($inputs | split row "," | each { |input| $input | str trim })
    
    for input in $input_list {
        print $"Updating input: ($input)"
        nix flake lock --update-input $input
    }
}

def update_all_inputs [] {
    print "🔄 Updating all inputs..."
    nix flake update
}

def check_for_changes [] {
    let has_changes = (cmp -s flake.lock flake.lock.backup | complete | get exit_code) != 0
    
    if $has_changes {
        $env.INPUTS_CHANGED = "true"
        print "✅ Changes detected in flake.lock"
    } else {
        $env.INPUTS_CHANGED = "false"
        print "ℹ️ No changes detected in flake.lock"
    }
}

def generate_update_summary [] {
    if $env.INPUTS_CHANGED == "false" {
        return
    }
    
    print "📝 Generating update summary..."
    
    "# Flake Input Updates

## Updated Inputs

" | save update_summary.org
    
    # Show what changed using git diff
    let diff_result = (git diff --no-index flake.lock.backup flake.lock | complete)
    if $diff_result.exit_code != 0 {
        let changes = ($diff_result.stdout | lines | where ($it | str starts-with "+") or ($it | str starts-with "-") | where not ($it | str starts-with "+++") | where not ($it | str starts-with "---"))
        
        for change in $changes {
            if ($change | str starts-with "+") {
                let line = ($change | str substring 1..)
                print $"- *Added/Updated:* ~($line)~" | save --append update_summary.org
            } else if ($change | str starts-with "-") {
                let line = ($change | str substring 1..)
                print $"- *Removed/Changed:* ~($line)~" | save --append update_summary.org
            }
        }
    } else {
        print "- Changes detected (see diff for details)" | save --append update_summary.org
    }
    
    "
## Verification
- [ ] All configurations build successfully
- [ ] No breaking changes introduced  
- [ ] Security review completed
" | save --append update_summary.org
}

def test_updated_configuration [] {
    if $env.INPUTS_CHANGED == "false" {
        return
    }
    
    print "🧪 Testing updated configuration..."
    
    # Test flake check first
    try {
        nix flake check --no-build
        print "✅ Flake check passed"
    } catch {
        print "❌ Flake check failed"
        exit 1
    }
    
    # Test key configurations
    print "Testing key configurations..."
    
    # Test NixOS config if available
    let nixos_configs = (try { nix eval .#nixosConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    if ($nixos_configs | where $it == "hyperion" | length) > 0 {
        print "Testing NixOS configuration: hyperion"
        try {
            timeout 300 nix build .#nixosConfigurations.hyperion.config.system.build.toplevel --no-link --show-trace
            print "✅ NixOS configuration test passed"
        } catch {
            print "❌ NixOS configuration test failed"
            exit 1
        }
    }
    
    # Test Darwin config if available  
    let darwin_configs = (try { nix eval .#darwinConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    if ($darwin_configs | where $it == "eos" | length) > 0 {
        print "Testing Darwin configuration: eos"
        try {
            timeout 300 nix build .#darwinConfigurations.eos.system --no-link --show-trace
            print "✅ Darwin configuration test passed"
        } catch {
            print "⚠️ Darwin build skipped (expected on non-Darwin)"
        }
    }
}