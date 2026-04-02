{
  description = "Emacs Lisp template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }: let
    projectName = "CHANGE ME!";
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [];
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
      ];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        formatter = pkgs.alejandra;
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            emacs
            nodejs-25
            eask-cli
          ];
        };
      };
    };
}
