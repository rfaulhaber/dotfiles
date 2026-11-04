# Temporary overlay: opencode 2.2.5's node_modules fixed-output derivation
# fails its hash check, taking the whole home-manager generation with it:
#
#   error: hash mismatch in fixed-output derivation '...-opencode-node_modules-2.2.5.drv':
#            specified: sha256-xfOVcw1+waLVNhFDwzOtzxrUSQEogvn88YomydOlhwc=
#               got:    sha256-prx9dX7kUvs/E3zopkaPk5LXh3xJAy5sZeum/W8L7cA=
#
# The 2.2.1 -> 2.2.5 bump (NixOS/nixpkgs@9c4790e0e8) landed with a wrong
# outputHash for the `bun install` output. The hash is shared by every
# platform, since the install fetches all of them (`--os="*" --cpu="*"`).
# https://github.com/NixOS/nixpkgs/issues/582673
#
# Mirrors the one-line fix in NixOS/nixpkgs#582675.
# https://github.com/NixOS/nixpkgs/pull/582675
#
# Self-disabling: once the pinned nixpkgs no longer declares the broken hash
# (the PR has landed, or opencode moved on), opencode passes through untouched
# and the eval warning flips to a removal reminder.
# https://nixpk.gs/pr-tracker.html?pr=582675
(import ../lib/temporary-overlay.nix).mkTemporaryOverlay {
  id = "opencode_20261104";
  attrPath = ["opencode"];
  fix = _final: prev: let
    brokenHash = "sha256-xfOVcw1+waLVNhFDwzOtzxrUSQEogvn88YomydOlhwc=";
    declared = prev.opencode.node_modules.outputHash;
  in {
    needed = declared == brokenHash;
    reason = "nixpkgs opencode ${prev.opencode.version} declares node_modules hash ${declared}";
    patched = prev.opencode.overrideAttrs (old: {
      node_modules = old.node_modules.overrideAttrs {
        outputHash = "sha256-prx9dX7kUvs/E3zopkaPk5LXh3xJAy5sZeum/W8L7cA=";
      };
    });
  };
}
