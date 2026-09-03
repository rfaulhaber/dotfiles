# Temporary overlay: flashrom 1.8.0 fails its own test suite on aarch64-linux.
# raspberrypi-eeprom puts flashrom on rpi-eeprom-update's PATH, so the failure
# takes the pallas and prometheus toplevels down with it:
#
#   flashrom> flashrom_image_read called: [  ERROR   ] --- -3 != 2
#   flashrom> [   LINE   ] --- ../tests/chip.c:816: error: Failure!
#   flashrom> [  FAILED  ] read_chip_bad_status_test
#   flashrom> Dummyflasher initialising with param=""... [  ERROR   ] --- 0 != -201
#   flashrom> [   LINE   ] --- ../tests/chip.c:133: error: Failure!
#   flashrom> [  FAILED  ] write_chip_bad_status_test
#   flashrom> ERROR: cmocka_group_tests leaked 9 block(s)
#   error: Cannot build '/nix/store/...-flashrom-1.8.0.drv'.
#
# NixOS/nixpkgs#557217 bumped flashrom 1.7.0 -> 1.8.0. On aarch64 the cmocka
# dummyflasher bad-status tests fail (which of the two fails varies between
# runs) and Hydra reproduces it, so there is no substitute to fall back on. The
# package runs the suite unconditionally on Linux.
# https://github.com/NixOS/nixpkgs/issues/558302
#
# Nothing is backported — no fix exists upstream yet. The suite is simply
# skipped on aarch64; every other platform keeps its tests. The exposure is
# small: rpi-eeprom-update only shells out to flashrom when
# RPI_EEPROM_IMMEDIATE_UPDATE=1, which it defaults to off on every board, so
# neither Pi runs the binary on its normal recovery.bin update path.
#
# Remove once the pinned nixpkgs ships a flashrom whose aarch64 suite passes;
# #558302 is the thing to watch.
#
# Self-disabling: the skip applies only while nixpkgs' flashrom is exactly
# 1.8.0 on an aarch64 host. Any version bump passes flashrom through untouched
# and flips the eval warning into a removal reminder — if that bump still
# fails, re-date the overlay rather than widening the condition.
_final: prev: let
  inherit (prev) lib;

  overlayNeeded =
    prev.flashrom.version
    == "1.8.0"
    && prev.stdenv.hostPlatform.isAarch64;
in {
  flashrom =
    if overlayNeeded
    then
      lib.warn "temporary overlay for flashrom enabled: flashrom_20260903" (prev.flashrom.overrideAttrs (_old: {
        doCheck = false;
      }))
    else
      lib.warn "overlay flashrom_20260903 is superfluous (flashrom ${prev.flashrom.version} on ${prev.stdenv.hostPlatform.system}): remove nix/overlays/flashrom_20260903.nix and its entries in flake.nix"
      prev.flashrom;
}
