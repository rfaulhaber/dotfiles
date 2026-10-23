# Builder for the dated workaround overlays in nix/overlays/ (conventions in
# nix/overlays/README.org). Deliberately free of `lib` at this level so overlay
# files can import it by relative path: overlays are also applied outside
# lib.my's reach, e.g. to the flake's perSystem pkgs.
{
  # Patches one attribute while `needed` holds and passes it through untouched
  # once it stops holding, recording which of the two happened under
  # `pkgs.temporaryOverlays.<id>` for `temporaryOverlayWarnings` to report.
  #
  # `fix` receives `final: prev:` and returns
  #   needed  — whether the workaround is still required; decide it from
  #             `prev`, since `final` can reach this overlay's own output
  #   patched — the replacement value for `attrPath`
  #   reason  — optional; what the check saw, shown once `needed` is false
  #   applies — optional, default true; when false (e.g. a platform the
  #             breakage never reached) the overlay is silent and inert
  mkTemporaryOverlay = {
    id,
    attrPath,
    fix,
  }:
    if builtins.match ".+_[0-9]{8}" id == null
    then throw "temporary overlay id '${id}' must be <package>_<YYYYMMDD>, matching its file stem"
    else
      final: prev: let
        inherit (prev) lib;
        r =
          {
            applies = true;
            reason = null;
          }
          // fix final prev;
        target = lib.showAttrPath attrPath;

        # Rebuilds only the sets along `attrPath`. The top-level key is fixed by
        # `attrPath` alone, so the check runs when that attribute is used rather
        # than whenever anything in pkgs is.
        update = path: orig:
          if path == []
          then
            if r.applies && r.needed
            then r.patched
            else orig
          else orig // {${builtins.head path} = update (builtins.tail path) orig.${builtins.head path};};
      in {
        ${builtins.head attrPath} = update (builtins.tail attrPath) prev.${builtins.head attrPath};

        temporaryOverlays =
          (prev.temporaryOverlays or {})
          // {
            ${id} =
              if !r.applies
              then null
              else if r.needed
              then "temporary overlay for ${target} enabled: ${id}"
              else "overlay ${id} is superfluous${lib.optionalString (r.reason != null) " (${r.reason})"}: remove ${id} from nix/overlays and every reference to it";
          };
      };

  # Messages from every temporary overlay applied to `pkgs`. Read them once per
  # evaluation (the host `warnings` module, a single perSystem output) rather
  # than through lib.warn on the patched attribute: that falls silent when
  # nothing forces the attribute, and repeats for every package set nixpkgs
  # re-applies overlays to (pkgsi686Linux, pkgsStatic, each `pkgs.extend`).
  temporaryOverlayWarnings = pkgs:
    builtins.filter (m: m != null) (builtins.attrValues (pkgs.temporaryOverlays or {}));
}
