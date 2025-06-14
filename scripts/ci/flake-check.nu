#!/usr/bin/env nu
# Flake validation and analysis script

def main [] {
    print "🔍 Starting flake validation..."
    
    # Check flake validity
    print "Checking flake validity..."
    try {
        nix flake check --all-systems --no-build
        print "✅ Flake check passed"
    } catch {
        print "❌ Flake check failed"
        exit 1
    }
    
    # Show flake info
    print "Generating flake information..."
    nix flake show
    
    print "✅ Flake validation completed successfully"
}