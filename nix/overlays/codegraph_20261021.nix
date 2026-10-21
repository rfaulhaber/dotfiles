# Temporary overlay: codegraph segfaults on every invocation, `--version`
# included, without writing anything to stderr:
#
#   $ codegraph --version
#   Segmentation fault (core dumped)
#
# Claude Code reports this from `codegraph prompt-hook` as "UserPromptSubmit
# hook error: Failed with non-blocking status code: No stderr output", and the
# codegraph MCP server fails to connect.
#
# codegraph ships a prebuilt non-PIE node binary that autoPatchelfHook
# rewrites. nixpkgs moved patchelf 0.15.2 -> 0.19.2, whose interpreter and
# RPATH rewrites leave the writable header LOAD overlapping an older read-only
# LOAD. `.dynamic` ends up mapped read-only and ld.so faults in dl_main writing
# DT_DEBUG, before node runs at all. nvidia-smi breaks the same way.
# https://github.com/NixOS/patchelf/issues/674
# https://github.com/NixOS/nixpkgs/issues/576412
#
# Mirrors NixOS/patchelf#672, merged after the 0.19.2 release, by building
# codegraph with a private patchelf carrying that commit. Only its src/ half is
# applied: the test half edits tests/Makefile.am, which the release tarball
# would need autoreconf for, and the existing suite still runs against the fix.
# The global patchelf, and every other package built with it, is untouched.
# https://github.com/NixOS/patchelf/pull/672
#
# Self-disabling: once nixpkgs ships a patchelf release past 0.19.2 (which will
# contain #672), codegraph passes through untouched and the eval warning flips
# to a removal reminder. If nixpkgs instead backports #672 onto 0.19.2, the
# patch below fails to apply as already present; remove the overlay then too.
final: prev: let
  inherit (prev) lib;

  overlayNeeded = prev.patchelf.version == "0.19.2";

  patchelf = prev.patchelf.overrideAttrs (old: {
    patches =
      (old.patches or [])
      ++ [
        (final.fetchpatch {
          name = "patchelf-fix-overlapping-load-mappings.patch";
          url = "https://github.com/NixOS/patchelf/commit/59acc3d98b4961cfddf02090d9426ac735603218.patch";
          includes = ["src/*"];
          hash = "sha256-rQWWZlCa4IRrcPcxMD2xrTTD4nLOsWfdZLOk9UoIOIs=";
        })
      ];
  });
in {
  codegraph =
    # Only the Linux builds go through autoPatchelfHook.
    if !prev.stdenv.hostPlatform.isLinux
    then prev.codegraph
    else if overlayNeeded
    then
      lib.warn "temporary overlay for codegraph enabled: codegraph_20261021" (prev.codegraph.overrideAttrs (old: {
        # auto-patchelf runs whichever `patchelf` is first on PATH, and
        # nativeBuildInputs come ahead of stdenv's own copy.
        nativeBuildInputs = old.nativeBuildInputs ++ [patchelf];
      }))
    else
      lib.warn "overlay codegraph_20261021 is superfluous (nixpkgs patchelf is ${prev.patchelf.version}): remove nix/overlays/codegraph_20261021.nix and its entries in flake.nix"
      prev.codegraph;
}
