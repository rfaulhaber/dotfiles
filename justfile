# Generate configs from Nix and copy to generated/
generate:
    nix build .#generated-configs --out-link .generated-result
    rm -rf generated/linux generated/darwin
    cp -rL .generated-result/* generated/
    unlink .generated-result
    @echo "Generated configs updated in generated/"

# Install configs on current (non-Nix) machine
install *ARGS:
    nu install.nu {{ ARGS }}
