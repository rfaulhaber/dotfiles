#!/usr/bin/env nu
# Code formatting validation script

def main [] {
    print "🔍 Checking code formatting..."
    
    # Determine which formatter to use
    let formatter = if (which nixfmt-classic | length) > 0 {
        "nixfmt-classic"
    } else if (try { nix run nixpkgs#nixfmt-classic -- --version } | complete | get exit_code) == 0 {
        "nix run nixpkgs#nixfmt-classic --"
    } else {
        print "⚠️ No nixfmt found, skipping formatting check"
        return
    }
    
    # Find all .nix files and check formatting
    let nix_files = (glob "**/*.nix" | where ($it | path type) == "file")
    let mut unformatted_files = []
    
    for file in $nix_files {
        let check_result = (run-external --redirect-stderr $formatter "--check" $file | complete)
        if $check_result.exit_code != 0 {
            $unformatted_files = ($unformatted_files | append $file)
        }
    }
    
    if ($unformatted_files | length) > 0 {
        print "❌ The following files are not properly formatted:"
        $unformatted_files | each { |file| print $"  - ($file)" }
        print ""
        print "Run 'nix fmt' to format all files."
        exit 1
    } else {
        print "✅ All Nix files are properly formatted."
    }
}