{
  description = "Rust project template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
      nixpkgs,
  }: let
    projectName = "Rust project template";
    supportedSystems = ["x86_64-linux" ];
    forSystems = systems: f:
      nixpkgs.lib.genAttrs systems
        (system: f system (import nixpkgs {inherit system;}));
    forAllSystems = forSystems supportedSystems;
  in {
    packages = forAllSystems (system: pkgs: {
      packages.${projectName} = pkgs.rustPlatform.buildRustPackage {
        pname = projectName;
        version = "0.1.0";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
      };
      default = self.packages.${system}.projectName;
    });

    formatter = forAllSystems (system: pkgs: pkgs.alejandra);

    devShells = forAllSystems (system: pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cargo
          rustc
          rustfmt
          clippy
          rust-analyzer
          rustup
        ];
      };
    });
  };
}
