# Temporary overlay: the pinned firefox-devedition (152.0b8) no longer builds.
# Mozilla prunes superseded betas from mirror://mozilla/devedition/releases/, so
# its fixed-output source fetch 404s, and 153.x otherwise fails to link with
# `ld.lld: error: undefined symbol: FREEBL_GetVector` on --with-system-nss builds.
#
# Backports NixOS/nixpkgs#540902: bump to 153.0b11 and add the upstream Mozilla
# fix that links freebl explicitly for system-nss builds.
# https://bugzilla.mozilla.org/show_bug.cgi?id=2047651
#
# Remove once a flake update pulls a nixpkgs containing #540902.
# https://github.com/NixOS/nixpkgs/pull/540902
#
# Track for PR merge status here: https://nixpk.gs/pr-tracker.html?pr=540902
final: prev: let
  version = "153.0b11";
in {
  firefox-devedition-unwrapped = prev.firefox-devedition-unwrapped.overrideAttrs (old: {
    inherit version;

    src = final.fetchurl {
      url = "mirror://mozilla/devedition/releases/${version}/source/firefox-${version}.source.tar.xz";
      sha512 = "b9cba9de51157db94ae421fcb82e2172e3a3b5026b01b1049c858e45302603dde9e8d859a163d1ec00e225788516fb769c1b7b1a4bd4f4ed3785c9552aab4e78";
    };

    # buildMozillaMach also stashes the version in passthru, captured lexically
    # from the original 152.0b8 argument, so the top-level override above leaves
    # `.version` reading stale unless it is refreshed here too.
    passthru = old.passthru // {inherit version;};

    # buildMozillaMach evaluates its version-gated `patches` list when called with
    # the original 152.0b8 argument; overrideAttrs bumps `version` afterward but
    # can't re-run those conditionals. 152.0b8 and 153.0b11 resolve to the same
    # patch set apart from this 153-only fix, so append it by hand.
    patches =
      (old.patches or [])
      ++ [
        (final.fetchpatch {
          name = "link-freebl-explicitly-for-system-nss-builds.patch";
          url = "https://hg-edge.mozilla.org/mozilla-central/raw-rev/1a56071ddc0fe97a55c3b825e1dd33c8422b9fc1";
          hash = "sha256-+HiU7RMPmV7I7SIzjP0Q6iSDJL/vBjc3UcwUTg57lNQ=";
        })
      ];
  });
}
