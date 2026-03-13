# Temporary fix: PR #496370 removed the absolute Exec path from the
# netbird-ui desktop file, breaking the NixOS netbird wrapper module.
# Fixed upstream in PR #499273 — remove this overlay after the next flake update.
# See: https://nixpk.gs/pr-tracker.html?pr=499273
(final: prev: {
  netbird-ui = prev.netbird-ui.overrideAttrs (oldAttrs: {
    postInstall =
      (oldAttrs.postInstall or "")
      + ''
        substituteInPlace $out/share/applications/netbird.desktop \
          --replace-quiet "Exec=netbird-ui" "Exec=$out/bin/netbird-ui"
      '';
  });
})
