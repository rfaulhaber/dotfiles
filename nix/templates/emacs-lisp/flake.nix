{
  description = "Emacs Lisp template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      projectName = "package";
    in rec {
      defaultPackage = pkgs.stdenv.mkDerivation {};
      # apps.${projectName} = flake-utils.lib.mkApp { drv = pkgs.${projectName}; };
      # defaultApp = apps.${projectName}};
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          emacs
          nodejs-22_x
          nodePackages_latest.eask
        ];
      };
    });
}
