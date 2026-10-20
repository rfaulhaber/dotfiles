# Temporary: distrust the roots anchoring eMudhra's compromised intermediates.
#
# Private keys for eight eMudhra intermediate CAs leaked (reported 2026-10-17).
# They chain to emSign ECC Root CA - G3 through a cross-signed emSign Root TLS
# CA - G3, and to Comodo AAA through Sectigo cross-signs, so a key holder can
# mint certificates that any client trusting those roots accepts. Browsers were
# covered within hours by OneCRL/CRLSets, but nothing that reads the system PEM
# bundle (curl, OpenSSL, Go, Python, nix's own fetchers) checks revocation:
# removing the anchors is the only lever.
# https://emudhra.we-have-a-problem.com/
#
# Mirrors the nixpkgs fixes. NixOS/nixpkgs#576390 hard-codes all four roots
# into the NixOS CA module's blacklist; NixOS/nixpkgs#576347 then bumps cacert
# to NSS 3.131.1, which drops the three emSign roots from certdata (NSS bugs
# 2084101 and 2083709), and keeps only Comodo AAA in that list. nix-darwin's
# PKI module has no equivalent.
# https://nixpk.gs/pr-tracker.html?pr=576390
# https://nixpk.gs/pr-tracker.html?pr=576347
#
# Self-disabling per root: a root is no longer added once the pinned platform
# distrusts it on its own, either by naming it in its PKI module or, for the
# emSign roots, by shipping a cacert >= 3.131.1. When none remain, the eval
# warning flips to a removal reminder. On eos the Comodo AAA entry stays until
# nix-darwin carries the same entry.
#
# Both PKI modules append their entries inside a `let` before calling
# `cacert.override`, so the option's value never shows them; the declaring
# module's source is the only place to look.
{
  lib,
  options,
  pkgs,
  ...
}: let
  id = "emudhra_distrust_20261020";

  nssDropped = [
    "emSign ECC Root CA - G3"
    "emSign Root CA - C1"
    "emSign ECC Root CA - C3"
  ];
  roots = nssDropped ++ ["Comodo AAA Services root"];

  # nix-darwin declares the option from a directory module.
  pkiModuleSource =
    lib.concatMapStrings (
      decl:
        builtins.readFile (
          if builtins.readFileType decl == "directory"
          then "${decl}/default.nix"
          else decl
        )
    )
    options.security.pki.caCertificateBlacklist.declarations;

  distrustedUpstream = name:
    lib.hasInfix ''"${name}"'' pkiModuleSource
    || (lib.elem name nssDropped && lib.versionAtLeast pkgs.cacert.version "3.131.1");

  pending = lib.filter (name: !distrustedUpstream name) roots;
in {
  security.pki.caCertificateBlacklist = pending;

  warnings = [
    (
      if pending != []
      then "temporary CA distrust enabled: ${id} (${lib.concatStringsSep ", " pending})"
      else "${id} is superfluous (the pinned nixpkgs/nix-darwin already distrusts every eMudhra-linked root): remove nix/modules/${id}.nix and its import in nix/modules/default.nix"
    )
  ];
}
