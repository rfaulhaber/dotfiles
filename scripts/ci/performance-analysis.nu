#!/usr/bin/env nu
# Performance analysis and reporting script

def main [] {
    print "⚡ Starting performance analysis..."
    
    analyze_build_times
    analyze_flake_performance
    analyze_dependencies
    analyze_cache_effectiveness
    
    print "✅ Performance analysis completed"
}

def analyze_build_times [] {
    print "📊 Analyzing build times..."
    
    let current_date = (date now | format date "%Y-%m-%d %H:%M:%S")
    
    $"#+TITLE: Build Performance Analysis
#+AUTHOR: Generated Performance Report
#+DATE: ($current_date)

* Build Performance Analysis

Generated on: ($current_date)

** Build Time Analysis

| Configuration | Build Time | Status |
|---------------|------------|--------|
" | save performance_report.org
    
    # Test configurations
    let configs_to_test = ["hyperion", "eos"]
    
    for config in $configs_to_test {
        print $"Testing ($config)..."
        
        # Try NixOS first
        let nixos_exists = (try { nix eval $".#nixosConfigurations.($config)" | complete | get exit_code } catch { 1 }) == 0
        if $nixos_exists {
            let start_time = (date now)
            let build_result = (timeout 600 nix build $".#nixosConfigurations.($config).config.system.build.toplevel" --no-link | complete)
            let end_time = (date now)
            
            if $build_result.exit_code == 0 {
                let duration = (($end_time - $start_time) / 1sec | math round)
                print $"| ($config) (NixOS) | ($duration)s | ✅ Success |" | save --append performance_report.org
            } else {
                print $"| ($config) (NixOS) | Timeout/Error | ❌ Failed |" | save --append performance_report.org
            }
        } else {
            # Try Darwin
            let darwin_exists = (try { nix eval $".#darwinConfigurations.($config)" | complete | get exit_code } catch { 1 }) == 0
            if $darwin_exists {
                let start_time = (date now)
                let build_result = (timeout 600 nix build $".#darwinConfigurations.($config).system" --no-link | complete)
                let end_time = (date now)
                
                if $build_result.exit_code == 0 {
                    let duration = (($end_time - $start_time) / 1sec | math round)
                    print $"| ($config) (Darwin) | ($duration)s | ✅ Success |" | save --append performance_report.org
                } else {
                    print $"| ($config) (Darwin) | Timeout/Error | ❌ Failed |" | save --append performance_report.org
                }
            } else {
                print $"| ($config) | N/A | ⚠️ Not Found |" | save --append performance_report.org
            }
        }
    }
}

def analyze_flake_performance [] {
    print "📊 Analyzing flake evaluation performance..."
    
    "
** Flake Evaluation Performance

" | save --append performance_report.org
    
    # Measure flake show time
    let start_time = (date now)
    nix flake show --quiet out+err> /dev/null
    let end_time = (date now)
    let duration_ms = (($end_time - $start_time) / 1ms | math round)
    
    print $"- *Flake evaluation time*: ($duration_ms)ms" | save --append performance_report.org
    
    # Measure flake check time
    let start_time = (date now)
    nix flake check --no-build out+err> /dev/null
    let end_time = (date now)
    let duration_ms = (($end_time - $start_time) / 1ms | math round)
    
    print $"- *Flake check time*: ($duration_ms)ms" | save --append performance_report.org
}

def analyze_dependencies [] {
    print "📊 Analyzing dependency metrics..."
    
    "
** Dependency Analysis

" | save --append performance_report.org
    
    # Count total dependencies
    let metadata = (nix flake metadata --json | from json)
    let total_inputs = ($metadata.locks.nodes | columns | length)
    print $"- *Total flake inputs*: ($total_inputs)" | save --append performance_report.org
    
    # Analyze package count for sample configs
    let nixos_configs = (try { nix eval .#nixosConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    if ($nixos_configs | where $it == "hyperion" | length) > 0 {
        let pkg_count = (try { 
            nix eval .#nixosConfigurations.hyperion.config.environment.systemPackages --apply "pkgs: builtins.length pkgs" 
        } catch { 
            "N/A" 
        })
        print $"- *System packages (hyperion)*: ($pkg_count)" | save --append performance_report.org
    }
}

def analyze_cache_effectiveness [] {
    print "📊 Analyzing cache effectiveness..."
    
    "
* Cache Analysis Report

Testing cache effectiveness...

" | save --append performance_report.org
    
    # Clear any local cache
    try { nix store gc } catch { }
    
    # Test cache hits for a simple build
    let nixos_configs = (try { nix eval .#nixosConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    if ($nixos_configs | where $it == "hyperion" | length) > 0 {
        print "Building hyperion to test cache hits..." | save --append performance_report.org
        
        let start_time = (date now)
        let build_result = (nix build .#nixosConfigurations.hyperion.config.system.build.toplevel --no-link --print-build-logs | complete)
        let end_time = (date now)
        
        let activity_count = ($build_result.stderr | lines | where ($it | str contains "copying path") or ($it | str contains "building") | length)
        let duration = (($end_time - $start_time) / 1sec | math round)
        
        print $"- *Build duration*: ($duration)s" | save --append performance_report.org
        print $"- *Build/copy operations*: ($activity_count)" | save --append performance_report.org
        
        if $activity_count < 10 {
            print "- *Cache efficiency*: 🟢 Excellent (most artifacts cached)" | save --append performance_report.org
        } else if $activity_count < 50 {
            print "- *Cache efficiency*: 🟡 Good (some cache misses)" | save --append performance_report.org
        } else {
            print "- *Cache efficiency*: 🔴 Poor (many cache misses)" | save --append performance_report.org
        }
    }
}