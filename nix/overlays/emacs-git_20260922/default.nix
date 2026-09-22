# Temporary overlay: emacs-git (the Emacs master snapshot from emacs-overlay)
# can no longer build a Doom profile. `doom build-profile` dies on the first
# file it scans for autoload cookies:
#
#   x There was an unexpected runtime error
#     Message: Error in Doom's autoloads file
#     Details: (".../doom-profile/profile/nix/0/init.32.0.el" (error
#       ".../lisp/doom-docs.el:0:0: error: cl-assertion-failed: ((> (point) output-start))"))
#
# Emacs commit 085e2d33a3 (2026-09-19, "loaddefs-gen.el: Fix (auto)loading
# files that are not (yet) in load-path") gave `loaddefs-generate--make-autoload'
# a third required argument, the full file name. lisp/obsolete/autoload.el still
# exposes that function through a bare `(defalias 'make-autoload ...)' and calls
# it with two arguments, so every call now signals wrong-number-of-arguments.
# The one inside `autoload-generate-file-autoloads' is swallowed as an "autoload
# cookie error", leaves the output buffer empty, and trips the assertion above.
# Doom's loaddefs generator (lisp/cli/loaddefs.el) is hit through both paths:
# it scans files with `autoload-generate-file-autoloads' and, for `;;;###autodef'
# cookies, calls the two-argument `make-autoload' directly.
# https://github.com/emacs-mirror/emacs/commit/085e2d33a3
#
# No fix upstream yet, in Emacs master or in Doom core (both checked
# 2026-09-22). The patch restores `make-autoload' as a two-argument wrapper that
# derives the file name the way the old code did (FILE + ".el"), and passes the
# real file name at the internal call site, which is what the upstream commit
# intended.
#
# Remove once Emacs master's lisp/obsolete/autoload.el is fixed past
# 2026-09-19 *and* Doom no longer calls the two-argument `make-autoload'
# (or has moved to `loaddefs-generate'), or once hyperion stops building Doom
# on emacs-git. The patch stops applying as soon as upstream touches either
# hunk, so a failing emacs-git build after a flake update is the cue to
# re-check both.
#
# Expects prev.emacs-git to be emacs-overlay's package. The emacs module
# applies that overlay through nixpkgs.overlays, and mkHost's overlays
# argument is merged after module-level overlays, so wiring this from
# flake.nix puts it in the right order.
#
# Self-disabling: only snapshots taken on or after 2026-09-19 carry the arity
# change. On an older snapshot the patch would itself be the breakage (the
# wrapper would hand four arguments to a three-argument function), so
# emacs-git passes through untouched and the eval warning flips to a removal
# reminder.
_final: prev: let
  inherit (prev) lib;

  overlayNeeded = lib.versionAtLeast prev.emacs-git.version "20260919";
in {
  emacs-git =
    if overlayNeeded
    then
      lib.warn "temporary overlay for emacs-git enabled: emacs-git_20260922" (prev.emacs-git.overrideAttrs (old: {
        patches = (old.patches or []) ++ [./make-autoload-arity.patch];
      }))
    else
      lib.warn "overlay emacs-git_20260922 is superfluous (emacs-git ${prev.emacs-git.version} predates the loaddefs-gen arity change): remove nix/overlays/emacs-git_20260922 and its entry in flake.nix"
      prev.emacs-git;
}
