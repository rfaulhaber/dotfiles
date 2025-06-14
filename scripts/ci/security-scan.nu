#!/usr/bin/env nu
# Security scanning and analysis script

def main [] {
    print "🔒 Starting security analysis..."
    
    check_for_secrets
    check_network_config
    check_ssh_security
    check_crypto_settings
    check_file_permissions
    
    print "✅ Security analysis completed"
}

def check_for_secrets [] {
    print "🔍 Scanning for potential secrets in Nix files..."
    
    let secret_patterns = [
        'password\s*=\s*"[^"]+"'
        'apiKey\s*=\s*"[^"]+"'
        'token\s*=\s*"[^"]+"'
        'secret\s*=\s*"[^"]+"'
        'key\s*=\s*"[^"]+"'
        '-----BEGIN.*PRIVATE KEY-----'
        '[A-Za-z0-9+/]{40,}={0,2}'  # Base64 encoded strings
    ]
    
    let mut found_issues = false
    
    for pattern in $secret_patterns {
        let matches = (rg $pattern --type nix . | complete)
        if $matches.exit_code == 0 and ($matches.stdout | str trim | str length) > 0 {
            print $"⚠️ Potential secret found with pattern: ($pattern)"
            print $matches.stdout
            $found_issues = true
        }
    }
    
    if $found_issues {
        print "❌ Potential secrets found in configuration files!"
        print "Please review and ensure no actual secrets are committed."
        exit 1
    } else {
        print "✅ No obvious secrets found in configuration files."
    }
}

def check_network_config [] {
    print "🔍 Checking for hardcoded sensitive network information..."
    
    let ip_matches = (rg '192\.168\.|10\.|172\.(1[6-9]|2[0-9]|3[0-1])\.' --type nix . | complete)
    
    if $ip_matches.exit_code == 0 and ($ip_matches.stdout | str trim | str length) > 0 {
        let filtered = ($ip_matches.stdout | lines | where not ($it | str contains "# Example") | where not ($it | str contains "# TODO"))
        if ($filtered | length) > 0 {
            print "ℹ️ Found private IP addresses in configuration:"
            $filtered | first 20 | each { |line| print $"  ($line)" }
            print "Please ensure these are intentional and not sensitive."
        }
    }
}

def check_ssh_security [] {
    print "🔍 Validating SSH configurations for security..."
    
    let insecure_patterns = [
        "PasswordAuthentication.*yes"
        "PermitRootLogin.*yes"
        "Protocol.*1"
        "PermitEmptyPasswords.*yes"
    ]
    
    let mut found_insecure = false
    
    for pattern in $insecure_patterns {
        let matches = (rg -i $pattern --type nix . | complete)
        if $matches.exit_code == 0 and ($matches.stdout | str trim | str length) > 0 {
            print $"⚠️ Potentially insecure SSH setting: ($pattern)"
            print $matches.stdout
            $found_insecure = true
        }
    }
    
    if $found_insecure {
        print "❌ Insecure SSH configurations found!"
        exit 1
    } else {
        print "✅ SSH configurations appear secure."
    }
}

def check_crypto_settings [] {
    print "🔍 Checking for weak cryptographic configurations..."
    
    let weak_crypto_patterns = [
        "md5"
        "sha1" 
        "des"
        "rc4"
        'ssl.*v[12]'
        'tls.*v1\.[01]'
    ]
    
    let mut found_weak = false
    
    for pattern in $weak_crypto_patterns {
        let matches = (rg -i $pattern --type nix . | complete)
        if $matches.exit_code == 0 and ($matches.stdout | str trim | str length) > 0 {
            # Filter out comments and port configs
            let filtered = ($matches.stdout | lines | where not ($it | str contains "#") | where not ($it | str contains "allowedTCPPorts"))
            if ($filtered | length) > 0 {
                print $"⚠️ Potentially weak crypto setting: ($pattern)"
                $filtered | each { |line| print $"  ($line)" }
                $found_weak = true
            }
        }
    }
    
    if not $found_weak {
        print "✅ No obvious weak cryptographic configurations found."
    }
}

def check_file_permissions [] {
    print "🔍 Checking file permissions..."
    
    # Check for overly permissive files
    let permissive_files = (find . -type f \( -perm -o+w -o -perm -g+w \) -not -path "./.git/*" -not -name "*.org" | complete)
    
    if $permissive_files.exit_code == 0 and ($permissive_files.stdout | str trim | str length) > 0 {
        print "⚠️ Found files with potentially overly permissive permissions:"
        print $permissive_files.stdout
        print "Consider restricting write permissions for security"
    } else {
        print "✅ File permissions look good"
    }
    
    # Check for executable files that shouldn't be
    let executable_nix = (find . -name "*.nix" -type f -executable -not -path "./.git/*" | complete)
    
    if $executable_nix.exit_code == 0 and ($executable_nix.stdout | str trim | str length) > 0 {
        print "⚠️ Found executable .nix files:"
        print $executable_nix.stdout
        print "Nix files typically shouldn't be executable"
    } else {
        print "✅ No unexpected executable .nix files found"
    }
}