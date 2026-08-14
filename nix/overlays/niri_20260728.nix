# Temporary overlay: niri (built from the YaLTeR/niri flake input) no longer
# builds. nixpkgs bumped libdisplay-info 0.3.0 -> 0.4.0, and niri's Cargo.lock
# pins the libdisplay-info-sys 0.3.0 crate, whose build script hard-requires
# `libdisplay-info < 0.4.0` via pkg-config.
#
# Mirrors NixOS/nixpkgs#546004, which fixed nixpkgs' own niri by introducing
# libdisplay-info_0_3 and pinning niri to it. That fix can't reach this build:
# the flake input's package takes the default `libdisplay-info` from whatever
# nixpkgs it is built against.
# https://github.com/NixOS/nixpkgs/pull/546004
#
# The 0.3.0 build is injected into niri only; the global libdisplay-info stays
# at 0.4.0 for its other consumers.
#
# Expects prev.niri to be the flake input's package, i.e.
# inputs.niri.overlays.default must be applied before this overlay.
#
# Self-disabling: the pin only applies while niri's own Cargo.lock still locks
# libdisplay-info-sys < 0.4 against a nixpkgs libdisplay-info >= 0.4. Once a
# `nix flake update niri` pulls a rev without that conflict, niri passes
# through untouched and the eval warning flips to a removal reminder.
final: prev: let
  inherit (prev) lib;

  cargoLock = builtins.fromTOML (builtins.readFile "${prev.niri.src}/Cargo.lock");
  sysCrate = lib.findFirst (p: p.name == "libdisplay-info-sys") null cargoLock.package;
  overlayNeeded =
    sysCrate
    != null
    && lib.versionOlder sysCrate.version "0.4"
    && lib.versionAtLeast prev.libdisplay-info.version "0.4";

  libdisplay-info_0_3 = prev.libdisplay-info.overrideAttrs (_old: {
    version = "0.3.0";
    src = final.fetchFromGitLab {
      domain = "gitlab.freedesktop.org";
      owner = "emersion";
      repo = "libdisplay-info";
      rev = "0.3.0";
      hash = "sha256-nXf2KGovNKvcchlHlzKBkAOeySMJXgxMpbi5z9gLrdc=";
    };
  });
in {
  niri =
    if overlayNeeded
    then
      lib.warn "temporary overlay for niri enabled: niri_20260728" (prev.niri.override {
        libdisplay-info = libdisplay-info_0_3;
      })
    else
      lib.warn "overlay niri_20260728 is superfluous (niri locks libdisplay-info-sys ${
        if sysCrate == null
        then "(absent)"
        else sysCrate.version
      } against libdisplay-info ${prev.libdisplay-info.version}): remove nix/overlays/niri_20260728.nix and its entry in flake.nix"
      prev.niri;
}
