# TODO versions to try:
# - 25.2.1
# - 25.1.6 (current working)
# - 25.1.7
# - override to latest commit
# - override to last working commit (https://github.com/NixOS/nixpkgs/commit/4c3870b23dded4e75292be48bdb03cd870fb1719)
# see: https://github.com/NixOS/nixpkgs/commits/master/pkgs/development/libraries/mesa
(final: prev: {
  mesa = prev.mesa.overrideAttrs (oldAttrs: let
    version = "25.1.6";
  in {
    inherit version;

    src = prev.fetchFromGitLab {
      domain = "gitlab.freedesktop.org";
      owner = "mesa";
      repo = "mesa";
      rev = "mesa-${version}";
      hash = "sha256-SHYYezt2ez9awvIATEC6wVMZMuJUsOYXxlugs1Q6q7U=";
    };
  });
})
