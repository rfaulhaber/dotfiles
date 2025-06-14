#!/usr/bin/env nu
# Dependency analysis and reporting script

def main [] {
    print "📊 Starting dependency analysis..."
    
    analyze_flake_inputs
    check_outdated_inputs
    
    print "✅ Dependency analysis completed"
}

def analyze_flake_inputs [] {
    print "🔍 Analyzing flake inputs..."
    
    let metadata = (nix flake metadata --json | from json)
    
    print "## Flake Inputs Analysis" | save --append $env.GITHUB_STEP_SUMMARY
    print "| Input | URL | Revision |" | save --append $env.GITHUB_STEP_SUMMARY  
    print "|-------|-----|----------|" | save --append $env.GITHUB_STEP_SUMMARY
    
    $metadata.locks.nodes | items { |key, value|
        if $key != "root" and ($value | get original --ignore-errors) != null {
            let url = if ($value.original | get url --ignore-errors) != null {
                $value.original.url
            } else if ($value.original | get owner --ignore-errors) != null {
                $"($value.original.owner)/($value.original.repo)"
            } else {
                "N/A"
            }
            
            let revision = if ($value.locked | get rev --ignore-errors) != null {
                ($value.locked.rev | str substring 0..7)
            } else {
                "N/A"
            }
            
            print $"| ($key) | ($url) | ($revision) |" | save --append $env.GITHUB_STEP_SUMMARY
        }
    }
}

def check_outdated_inputs [] {
    print "🔍 Checking for outdated inputs..."
    
    print "## Input Update Check" | save --append $env.GITHUB_STEP_SUMMARY
    print "Checking if any flake inputs have updates available..." | save --append $env.GITHUB_STEP_SUMMARY
    
    # Create a backup of flake.lock
    cp flake.lock flake.lock.backup
    
    # Check for updates without writing
    let update_result = (nix flake update --no-write-lock-file | complete)
    
    if $update_result.exit_code == 0 {
        # Compare files to see if there would be changes
        let has_changes = (cmp -s flake.lock flake.lock.backup | complete | get exit_code) != 0
        
        if $has_changes {
            print "⚠️ Some inputs have updates available" | save --append $env.GITHUB_STEP_SUMMARY
        } else {
            print "✅ All inputs are up to date" | save --append $env.GITHUB_STEP_SUMMARY
        }
    } else {
        print "❌ Error checking for updates" | save --append $env.GITHUB_STEP_SUMMARY
    }
    
    # Restore backup
    mv flake.lock.backup flake.lock
}