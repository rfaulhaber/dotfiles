#!/usr/bin/env nu
# Configuration build testing script

def main [host: string, platform: string] {
    print $"🔨 Building ($platform) configuration for ($host)..."
    
    let start_time = (date now)
    
    try {
        match $platform {
            "nixos" => {
                nix build $".#nixosConfigurations.($host).config.system.build.toplevel" --print-build-logs --fallback
            }
            "darwin" => {
                nix build $".#darwinConfigurations.($host).system" --print-build-logs --fallback  
            }
            _ => {
                print $"❌ Unknown platform: ($platform)"
                exit 1
            }
        }
        
        let end_time = (date now)
        let duration = ($end_time - $start_time)
        print $"✅ Successfully built ($host) in ($duration)"
        
    } catch {
        print $"❌ Failed to build ($platform) configuration for ($host)"
        exit 1
    }
}