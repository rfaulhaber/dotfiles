{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {flake-parts, ...}: let
    projectName = "project";
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [];
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.rust-overlay.overlays.default
          ];
        };

        formatter = pkgs.alejandra;

        packages = {
          ${projectName} = pkgs.rustPlatform.buildRustPackage {
            pname = projectName;
            version = "0.1.0";
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
          };
          default = self'.packages.${projectName};
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # https://github.com/oxalica/rust-overlay?tab=readme-ov-file#cheat-sheet-common-usage-of-rust-bin
            rust-bin.stable.latest.default
            rust-analyzer
          ];
        };
      };
      flake = {};
    };
}
