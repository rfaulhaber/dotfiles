# Temporary overlay: nushellPlugins.polars fails to build against rustc 1.97.
#
#   error[E0512]: cannot transmute between types of different sizes, or dependently-sized types
#     --> /build/nu_plugin_polars-0.114.1-vendor/source-registry-0/ethnum-1.5.2/src/error.rs:16:14
#      |
#   16 |     unsafe { mem::transmute(()) }
#      |              ^^^^^^^^^^^^^^
#      = note: source type: `()` (0 bits)
#      = note: target type: `TryFromIntError` (8 bits)
#
# nushell 0.114.1's Cargo.lock pins ethnum 1.5.2, which fabricates a
# `TryFromIntError` by transmuting from `()`. That was always unsound and stops
# compiling outright on rustc >= 1.97, where the target type is no longer
# zero-sized. ethnum 1.5.3 replaces the transmute with a safe conversion.
# https://github.com/NixOS/nixpkgs/issues/546250
#
# Mirrors NixOS/nixpkgs#546343, which carries the same Cargo.lock bump nushell
# merged upstream in 1139611e188d.
# https://github.com/NixOS/nixpkgs/pull/546343
#
# Remove once that PR reaches the pinned nixpkgs:
# https://nixpk.gs/pr-tracker.html?pr=546343
#
# `cargoHash` and `cargoPatches` are consumed by buildRustPackage itself, not by
# mkDerivation, so overrideAttrs cannot reach them — setting `cargoHash` there
# would be silently ignored and keep the unpatched vendor tree. The two things
# `cargoPatches` does are therefore done by hand: the patch is appended to
# `patches` for the build, and `cargoDeps` is rebuilt from scratch so the
# vendored crates come from the corrected lock file.
#
# Self-disabling: the fix applies only while nushell's lock still pins the
# broken ethnum AND nixpkgs has not patched polars itself. Once either flips
# — a `nix flake update` pulling a newer nushell, or #546343 landing — polars
# passes through untouched and the eval warning becomes a removal reminder.
final: prev: let
  inherit (prev) lib;

  patch = ./ethnum.patch;

  cargoLock = builtins.fromTOML (builtins.readFile "${prev.nushell.src}/Cargo.lock");
  ethnum = lib.findFirst (p: p.name == "ethnum") null cargoLock.package;

  lockStillBroken = ethnum != null && lib.versionOlder ethnum.version "1.5.3";
  # Non-empty `patches` means nixpkgs' own cargoPatches fix has landed; applying
  # ours on top would fail to apply and invalidate the vendor hash below.
  nixpkgsAlreadyPatched = (prev.nushellPlugins.polars.patches or []) != [];
  overlayNeeded = lockStillBroken && !nixpkgsAlreadyPatched;

  patched = prev.nushellPlugins.polars.overrideAttrs (old: {
    patches = (old.patches or []) ++ [patch];
    cargoDeps = final.rustPlatform.fetchCargoVendor {
      inherit (old) pname version src;
      patches = [patch];
      hash = "sha256-Cpv58bqpx1o0Dz2AykqzFY+PQE/Updr5MusQflpEF74=";
    };
  });
in {
  nushellPlugins =
    prev.nushellPlugins
    // {
      polars =
        if overlayNeeded
        then lib.warn "temporary overlay for nushellPlugins.polars enabled: nushell_polars_20260805" patched
        else
          lib.warn "overlay nushell_polars_20260805 is superfluous (${
            if nixpkgsAlreadyPatched
            then "nixpkgs patches polars itself"
            else "nushell locks ethnum ${
              if ethnum == null
              then "(absent)"
              else ethnum.version
            }"
          }): remove nix/overlays/nushell_polars_20260805/ and its entry in flake.nix"
          prev.nushellPlugins.polars;
    };
}
