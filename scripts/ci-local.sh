#!/usr/bin/env bash
# Local CI testing script for dotfiles
# Run this script to test your configuration locally before pushing

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to run a test and report results
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    log_info "Running: $test_name"
    
    if eval "$test_command"; then
        log_success "$test_name passed"
        return 0
    else
        log_error "$test_name failed"
        return 1
    fi
}

# Check if we're in the right directory
if [[ ! -f "flake.nix" ]]; then
    log_error "flake.nix not found. Please run this script from the root of your dotfiles repository."
    exit 1
fi

log_info "Starting local CI checks for dotfiles..."
echo

failed_tests=0
total_tests=0

# Test 1: Flake check
((total_tests++))
if ! run_test "Flake validity check" "nix flake check --no-build"; then
    ((failed_tests++))
fi
echo

# Test 2: Flake show
((total_tests++))
if ! run_test "Flake show" "nix flake show"; then
    ((failed_tests++))
fi
echo

# Test 3: Formatting check
((total_tests++))
format_check() {
    if command -v nixfmt-classic >/dev/null 2>&1; then
        FORMATTER="nixfmt-classic"
    elif nix run nixpkgs#nixfmt-classic -- --version >/dev/null 2>&1; then
        FORMATTER="nix run nixpkgs#nixfmt-classic --"
    else
        log_warning "No nixfmt found, skipping formatting check"
        return 0
    fi
    
    unformatted_files=()
    while IFS= read -r -d '' file; do
        if ! $FORMATTER --check "$file" >/dev/null 2>&1; then
            unformatted_files+=("$file")
        fi
    done < <(find . -name "*.nix" -type f -print0)
    
    if [ ${#unformatted_files[@]} -gt 0 ]; then
        log_error "The following files are not properly formatted:"
        printf '%s\n' "${unformatted_files[@]}"
        log_info "Run 'nix fmt' to format all files."
        return 1
    fi
    return 0
}

if ! run_test "Formatting check" "format_check"; then
    ((failed_tests++))
fi
echo

# Test 4: Dead code check
((total_tests++))
if command -v deadnix >/dev/null 2>&1 || nix run nixpkgs#deadnix -- --version >/dev/null 2>&1; then
    if ! run_test "Dead code check" "nix run nixpkgs#deadnix -- --fail --no-lambda-arg --no-lambda-pattern-names ."; then
        ((failed_tests++))
    fi
else
    log_warning "deadnix not available, skipping dead code check"
fi
echo

# Test 5: Static analysis
((total_tests++))
if command -v statix >/dev/null 2>&1 || nix run nixpkgs#statix -- --version >/dev/null 2>&1; then
    if ! run_test "Static analysis" "nix run nixpkgs#statix -- check ."; then
        ((failed_tests++))
    fi
else
    log_warning "statix not available, skipping static analysis"
fi
echo

# Test 6: Build test for available configurations
log_info "Testing configuration builds..."

# Get available configurations
nixos_configs=()
darwin_configs=()

if nix eval .#nixosConfigurations --apply builtins.attrNames >/dev/null 2>&1; then
    while IFS= read -r config; do
        nixos_configs+=("$config")
    done < <(nix eval .#nixosConfigurations --apply builtins.attrNames --json 2>/dev/null | jq -r '.[]' 2>/dev/null || true)
fi

if nix eval .#darwinConfigurations --apply builtins.attrNames >/dev/null 2>&1; then
    while IFS= read -r config; do
        darwin_configs+=("$config")
    done < <(nix eval .#darwinConfigurations --apply builtins.attrNames --json 2>/dev/null | jq -r '.[]' 2>/dev/null || true)
fi

# Test NixOS configurations
for config in "${nixos_configs[@]}"; do
    ((total_tests++))
    if ! run_test "Build NixOS config: $config" "timeout 300 nix build .#nixosConfigurations.$config.config.system.build.toplevel --no-link"; then
        ((failed_tests++))
    fi
done

# Test Darwin configurations (may fail on non-Darwin systems)
for config in "${darwin_configs[@]}"; do
    ((total_tests++))
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if ! run_test "Build Darwin config: $config" "timeout 300 nix build .#darwinConfigurations.$config.system --no-link"; then
            ((failed_tests++))
        fi
    else
        log_warning "Skipping Darwin config $config (not on macOS)"
    fi
done

echo
echo "===================="
echo "CI Test Summary"
echo "===================="
echo "Total tests: $total_tests"
echo "Passed: $((total_tests - failed_tests))"
echo "Failed: $failed_tests"
echo

if [[ $failed_tests -eq 0 ]]; then
    log_success "All tests passed! 🎉"
    log_info "Your configuration is ready to push."
    exit 0
else
    log_error "$failed_tests test(s) failed."
    log_info "Please fix the issues above before pushing."
    exit 1
fi