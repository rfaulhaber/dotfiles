#!/usr/bin/env nu
# Documentation generation script

def main [] {
    print "📚 Generating documentation..."
    
    mkdir docs
    
    generate_main_docs
    generate_module_docs  
    generate_host_docs
    validate_readme
    
    print "✅ Documentation generation completed"
}

def generate_main_docs [] {
    print "📝 Generating main documentation..."
    
    let current_date = (date now | format date "%Y-%m-%d %H:%M:%S")
    
    $"#+TITLE: Dotfiles Configuration Documentation
#+AUTHOR: Generated Documentation
#+DATE: ($current_date)

* Available Configurations

Generated on: ($current_date)

" | save docs/README.org
    
    # Document NixOS configurations
    let nixos_configs = (try { nix eval .#nixosConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    if ($nixos_configs | length) > 0 {
        "** NixOS Configurations

" | save --append docs/README.org
        
        for host in $nixos_configs {
            print $"- *($host)*: NixOS configuration" | save --append docs/README.org
        }
        print "" | save --append docs/README.org
    }
    
    # Document Darwin configurations  
    let darwin_configs = (try { nix eval .#darwinConfigurations --apply builtins.attrNames --json | from json } catch { [] })
    if ($darwin_configs | length) > 0 {
        "** Darwin Configurations

" | save --append docs/README.org
        
        for host in $darwin_configs {
            print $"- *($host)*: macOS configuration" | save --append docs/README.org
        }
        print "" | save --append docs/README.org
    }
    
    # Document flake inputs
    "** Flake Inputs

| Input | Type | URL |
|-------|------|-----|
" | save --append docs/README.org
    
    let metadata = (nix flake metadata --json | from json)
    $metadata.locks.nodes | items { |key, value|
        if $key != "root" and ($value | get original --ignore-errors) != null {
            let input_type = ($value.original | get type --ignore-errors | default "unknown")
            let url = if ($value.original | get url --ignore-errors) != null {
                $value.original.url
            } else if ($value.original | get owner --ignore-errors) != null {
                $"($value.original.owner)/($value.original.repo)"
            } else {
                "N/A"
            }
            
            print $"| ($key) | ($input_type) | ($url) |" | save --append docs/README.org
        }
    }
    
    "
** Module Structure

#+BEGIN_SRC
" | save --append docs/README.org
    
    (find nix/modules -name "*.nix" | sort | first 20) | each { |file| print $file | save --append docs/README.org }
    
    "#+END_SRC
" | save --append docs/README.org
}

def generate_module_docs [] {
    print "📝 Generating module documentation..."
    
    let current_date = (date now | format date "%Y-%m-%d %H:%M:%S")
    
    $"#+TITLE: Module Documentation
#+AUTHOR: Generated Documentation  
#+DATE: ($current_date)

* Module Documentation

This document provides an overview of available modules.

" | save docs/modules.org
    
    # Document module structure
    let categories = ["programs", "services", "desktop", "themes"]
    
    for category in $categories {
        let category_path = $"nix/modules/($category)"
        if ($category_path | path exists) {
            print $"** ($category | str title-case) Modules

" | save --append docs/modules.org
            
            let modules = (find $category_path -name "*.nix" | where not ($it | str ends-with "default.nix") | sort)
            
            for module in $modules {
                let module_name = ($module | path basename | str replace ".nix" "")
                let relative_path = ($module | str replace "nix/modules/" "")
                print $"- *($module_name)*: ~($relative_path)~" | save --append docs/modules.org
                
                # Check for options in the module
                let has_options = (rg "options\." $module | complete | get exit_code) == 0
                if $has_options {
                    print "  - Configuration options available" | save --append docs/modules.org
                }
            }
            print "" | save --append docs/modules.org
        }
    }
}

def generate_host_docs [] {
    print "📝 Generating host documentation..."
    
    let current_date = (date now | format date "%Y-%m-%d %H:%M:%S")
    
    $"#+TITLE: Host Configurations
#+AUTHOR: Generated Documentation
#+DATE: ($current_date)

* Host Configurations

" | save docs/hosts.org
    
    let host_configs = (find nix/hosts -name "configuration.nix")
    
    for config in $host_configs {
        let host = ($config | path dirname | path basename)
        print $"** ($host)

" | save --append docs/hosts.org
        
        # Extract basic info from configuration
        let has_programs = (rg "programs\." $config | complete | get exit_code) == 0
        if $has_programs {
            "*Enabled Programs:*
" | save --append docs/hosts.org
            
            let programs = (rg '^\s*[a-zA-Z0-9_-]+\.enable = true;' $config | complete)
            if $programs.exit_code == 0 {
                ($programs.stdout | lines | first 10 | each { |line|
                    let prog = ($line | parse --regex '.*\.([a-zA-Z0-9_-]*)\.enable = true;' | get capture0.0? | default "unknown")
                    print $"- ($prog)"
                }) | save --append docs/hosts.org
            }
            print "" | save --append docs/hosts.org
        }
        
        let has_services = (rg "services\." $config | complete | get exit_code) == 0
        if $has_services {
            "*Enabled Services:*
" | save --append docs/hosts.org
            
            let services = (rg '^\s*[a-zA-Z0-9_-]+\.enable = true;' $config | rg services | complete)
            if $services.exit_code == 0 {
                ($services.stdout | lines | first 10 | each { |line|
                    let service = ($line | parse --regex '.*\.([a-zA-Z0-9_-]*)\.enable = true;' | get capture0.0? | default "unknown")
                    print $"- ($service)"
                }) | save --append docs/hosts.org
            }
            print "" | save --append docs/hosts.org
        }
    }
}

def validate_readme [] {
    print "📝 Validating README..."
    
    if not ("README.org" | path exists) and not ("README.md" | path exists) {
        print "❌ No README found (README.org or README.md)"
        print "Consider adding a README with:"
        print "- Project description"
        print "- Installation instructions"
        print "- Usage examples" 
        print "- Configuration overview"
        exit 1
    } else {
        print "✅ README found"
    }
    
    # Check for basic documentation sections in org or markdown
    let readme_file = if ("README.org" | path exists) { "README.org" } else { "README.md" }
    let required_sections = ["Installation", "Usage", "Configuration"]
    let mut missing_sections = []
    
    for section in $required_sections {
        let has_section = (rg $'[#*]+ ($section)' $readme_file | complete | get exit_code) == 0
        if not $has_section {
            $missing_sections = ($missing_sections | append $section)
        }
    }
    
    if ($missing_sections | length) > 0 {
        print $"ℹ️ Consider adding these sections to ($readme_file):"
        $missing_sections | each { |section| print $"- ($section)" }
    } else {
        print $"✅ ($readme_file) has good documentation structure"
    }
}