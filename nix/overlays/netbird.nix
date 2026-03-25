# Temporary overlay: netbird 0.65.3 uses buildGo125Module which was removed
# from nixpkgs. Bumps to 0.67.0 which includes the upstream fix for the
# gvisor build-tag conflict with Go 1.26 (netbirdio/netbird#5447).
# Fixed upstream in PR #502766 — remove this overlay after the next flake update.
# See: https://nixpk.gs/pr-tracker.html?pr=502766
#
# Also restores the absolute Exec path in the netbird-ui desktop file
# (PR #499273).
(final: prev: let
  version = "0.67.0";
  src = final.fetchFromGitHub {
    owner = "netbirdio";
    repo = "netbird";
    tag = "v${version}";
    hash = "sha256-5Q90bEAXTnvkEHcsheohu9wdwZRFIoLnqBNzjotFz54=";
  };
  vendorHash = "sha256-6qYS2jXjfPczAfv+g79JsTcEJR9FniAVjW52Yi/g42M=";
in {
  # Provide buildGo125Module as buildGoModule so the base package can evaluate.
  buildGo125Module = final.buildGoModule;

  netbird = prev.netbird.overrideAttrs (_: {
    inherit version src vendorHash;
  });

  netbird-ui =
    (prev.netbird-ui.overrideAttrs (_: {
      inherit version src vendorHash;
    })).overrideAttrs
    (old: {
      postInstall =
        (old.postInstall or "")
        + ''
          substituteInPlace $out/share/applications/netbird.desktop \
            --replace-quiet "Exec=netbird-ui" "Exec=$out/bin/netbird-ui"
        '';
    });
})
