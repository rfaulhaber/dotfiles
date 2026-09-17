# Temporary overlay: every host that enables sops fails to evaluate.
#
#   … while evaluating the option `sops.package':
#   error: Go 1.25 is end-of-life, and 'buildGo125Module' has been removed.
#   Please use a newer builder version.
#
# sops-nix builds sops-install-secrets with a hardcoded `buildGo125Module`.
# nixpkgs dropped Go 1.25 at EOL and turned that attribute into a `throw`
# (aliases.nix, 2026-09-15), so the package can no longer even be evaluated.
# Nothing is wrong in nixpkgs — the stale reference is sops-nix's.
# https://github.com/Mic92/sops-nix/issues/983
#
# Mirrors Mic92/sops-nix#984, which switches the package to the unversioned
# `buildGoModule`. That PR also raises go.mod to 1.26, which is not needed to
# build: the `go` directive is a minimum, so Go 1.26 compiles the 1.25 module
# unmodified and the vendor hash is unchanged.
# https://github.com/Mic92/sops-nix/pull/984
#
# Remove once that PR (or an equivalent) is merged and the sops-nix input is
# bumped past it.
#
# sops-install-secrets is never an attribute of pkgs — sops-nix's NixOS,
# nix-darwin and home-manager modules each instantiate it privately through
# `pkgs.callPackage`. The builder's name is therefore the only seam an overlay
# can reach, and restoring it repairs all three at once. Reviving the name
# package-set-wide is safe: nixpkgs may not reference its own aliases, so the
# only possible consumers are out-of-tree expressions that are equally stale.
#
# Shaped differently from the other overlays here, on two counts.
#
# It takes the sops-nix source as an argument, because the expression that
# decides whether this is still needed lives in that flake input rather than
# anywhere in `prev`.
#
# It returns `{ overlay, warning }` instead of a bare overlay, and the message
# is surfaced through the module system's `warnings` rather than `lib.warn`.
# `lib.warn` has nowhere correct to live in this overlay: on the alias it would
# fall silent once sops-nix is fixed, since the broken consumer is the only
# thing that ever forces the alias — exactly when it should become a removal
# reminder; on the overlay's own attrset it repeats dozens of times per
# evaluation, once for every derived package set nixpkgs re-applies overlays
# to (pkgsi686Linux, pkgsStatic, each `pkgs.extend`, ...). `warnings` is forced
# once per host regardless of either.
sops-nix: let
  consumer = "${sops-nix}/pkgs/sops-install-secrets/default.nix";

  # TODO: narrow this to "sops-nix still asks for buildGo125Module", so that a
  # flake update which pulls in the upstream fix flips the warning below.
  overlayNeeded = builtins.pathExists consumer;
in {
  overlay = final: _prev:
    if overlayNeeded
    then {buildGo125Module = final.buildGoModule;}
    else {};

  warning =
    if overlayNeeded
    then "temporary overlay for sops-install-secrets enabled: sops_install_secrets_20260917"
    else "overlay sops_install_secrets_20260917 is superfluous (sops-nix no longer asks for buildGo125Module): remove nix/overlays/sops_install_secrets_20260917.nix and its use in nix/modules/programs/sops/default.nix";
}
