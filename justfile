# Generate configs from Nix and copy to generated/
generate:
    nix build .#generated-configs --out-link .generated-result
    chmod -R u+w generated/linux generated/darwin 2>/dev/null || true
    rm -rf generated/linux generated/darwin
    cp -rL .generated-result/* generated/
    chmod -R u+w generated/linux generated/darwin
    unlink .generated-result
    @echo "Generated configs updated in generated/"

# Install configs on current (non-Nix) machine
install *ARGS:
    nu install.nu {{ ARGS }}
