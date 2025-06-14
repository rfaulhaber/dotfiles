#!/usr/bin/env nu
# Local CI testing script for dotfiles
# Run this script to test your configuration locally before pushing

def main [] {
    print "🚀 Starting local CI checks for dotfiles..."
    print ""
    
    # Check if we're in the right directory
    if not ("flake.nix" | path exists) {
        print "❌ flake.nix not found. Please run this script from the root of your dotfiles repository."
        exit 1
    }
    
    let mut failed_tests = 0
    let mut total_tests = 0
    
    # Test 1: Flake check
    $total_tests = $total_tests + 1
    if not (run_test "Flake validity check" { ^$env.PWD/scripts/ci/flake-check.nu }) {
        $failed_tests = $failed_tests + 1
    }
    print ""
    
    # Test 2: Formatting check
    $total_tests = $total_tests + 1
    if not (run_test "Formatting check" { ^$env.PWD/scripts/ci/format-check.nu }) {
        $failed_tests = $failed_tests + 1
    }
    print ""
    
    # Test 3: Dead code check
    $total_tests = $total_tests + 1
    if (which deadnix | length) > 0 or (try { nix run nixpkgs#deadnix -- --version } | complete | get exit_code) == 0 {
        if not (run_test "Dead code check" { nix run nixpkgs#deadnix -- --fail --no-lambda-arg --no-lambda-pattern-names . }) {
            $failed_tests = $failed_tests + 1
        }
    } else {
        print "⚠️ deadnix not available, skipping dead code check"
    }
    print ""
    
    # Test 4: Static analysis
    $total_tests = $total_tests + 1
    if (which statix | length) > 0 or (try { nix run nixpkgs#statix -- --version } | complete | get exit_code) == 0 {
        if not (run_test "Static analysis" { nix run nixpkgs#statix -- check . }) {
            $failed_tests = $failed_tests + 1
        }
    } else {
        print "⚠️ statix not available, skipping static analysis"
    }
    print ""
    
    # Test 5: Security scan
    $total_tests = $total_tests + 1
    if not (run_test "Security scan" { ^$env.PWD/scripts/ci/security-scan.nu }) {
        $failed_tests = $failed_tests + 1
    }
    print ""
    
    # Test 6: Build test for available configurations
    print "🔨 Testing configuration builds..."
    
    # Get available configurations
    let nixos_configs = (try { nix eval .#nixosConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    let darwin_configs = (try { nix eval .#darwinConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    
    # Test NixOS configurations
    for config in $nixos_configs {
        $total_tests = $total_tests + 1
        if not (run_test $"Build NixOS config: ($config)" { 
            ^$env.PWD/scripts/ci/build-config.nu $config "nixos" 
        }) {
            $failed_tests = $failed_tests + 1
        }
    }
    
    # Test Darwin configurations (may fail on non-Darwin systems)
    for config in $darwin_configs {
        $total_tests = $total_tests + 1
        if ($nu.os-info.name == "macos") {
            if not (run_test $"Build Darwin config: ($config)" { 
                ^$env.PWD/scripts/ci/build-config.nu $config "darwin" 
            }) {
                $failed_tests = $failed_tests + 1
            }
        } else {
            print $"⚠️ Skipping Darwin config ($config) (not on macOS)"
        }
    }
    
    print ""
    print "===================="
    print "CI Test Summary"
    print "===================="
    print $"Total tests: ($total_tests)"
    print $"Passed: ($total_tests - $failed_tests)"
    print $"Failed: ($failed_tests)"
    print ""
    
    if $failed_tests == 0 {
        print "✅ All tests passed! 🎉"
        print "Your configuration is ready to push."
        exit 0
    } else {
        print $"❌ ($failed_tests) test(s) failed."
        print "Please fix the issues above before pushing."
        exit 1
    }
}

def run_test [test_name: string, test_command: closure] {
    print $"🔍 Running: ($test_name)"
    
    let result = (try { do $test_command } catch { false })
    
    if $result != false {
        print $"✅ ($test_name) passed"
        return true
    } else {
        print $"❌ ($test_name) failed"
        return false
    }
}