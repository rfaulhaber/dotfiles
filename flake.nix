# this flake has gotten rather unwieldy and I need to refactor it severely
# a couple of improvements I need to make are:
# - refactor modules to not depend on pkgs
# - refactor configurations to be platform independent
# - refactor to allow the same host to be built for different targets, e.g. hyperion on a vm
{
  description = "My Nix system configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs.url = "github:serokell/deploy-rs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    flake-utils.url = "github:numtide/flake-utils";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # nixos-aarch64-images.url = "github:Mic92/nixos-aarch64-images";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    deploy-rs,
    nixos-hardware,
    flake-utils,
    nixos-generators,
    nix-darwin,
    ...
  }: let
    inherit (lib.my) mapModules mkPkgs;
    pkgs = mkPkgs nixpkgs [];

    # TODO import lib functions that don't depend on pkgs
    lib = nixpkgs.lib.extend (self: super: {
      my = import ./nix/lib {
        inherit inputs pkgs;
        lib = self;
      };
    });
  in
    {
      templates = {
        rust = {
          path = ./nix/templates/rust;
          description = "Rust project template";
        };
        emacs-lisp = {
          path = ./nix/templates/emacs-lisp;
          description = "Emacs Lisp template";
        };
      };

      # TODO utilize top-level nixosModules

      overlays =
        mapModules ./nix/overlays import;

      # these are the actual system configurations
      # any particular system can be build with nixos-rebuild of course, but also:
      # nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel

      # TODO refactor as packages, configurations can be built as packages
      # like so: packages.<platform>.nixosConfigurations.<hostname>.config.system.build.toplevel
      nixosConfigurations = let
        mkHost = lib.my.mkNixOSHost;
      in {
        hyperion = mkHost ./nix/hosts/hyperion/configuration.nix {
          system = "x86_64-linux";
        };
        atlas = mkHost ./nix/hosts/atlas/configuration.nix {
          system = "x86_64-linux";
        };
        helios = mkHost ./nix/hosts/helios/configuration.nix {
          system = "x86_64-linux";
        };
        pallas = mkHost ./nix/hosts/pallas/configuration.nix {
          system = "aarch64-linux";
        };
      };

      # TODO add darwin configurations

      # TODO write a mapHosts function, like here: https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix
      # nixosConfigurations = mapHosts ./nix/hosts { };

      # TODO make deployment nodes their own files
      deploy.nodes = {
        # run with: nix run '.#deploy-rs' '.#atlas'
        atlas = {
          hostname = "atlas";
          sshUser = "ryan";
          sshOpts = ["-t"];
          autoRollback = true;
          magicRollback = false;
          profiles.system = {
            user = "root";
            path =
              deploy-rs.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.atlas;
          };
        };
        pallas = {
          hostname = "pallas";
          sshUser = "ryan";
          sshOpts = ["-t"];
          autoRollback = true;
          magicRollback = false;
          profiles.system = {
            user = "root";
            path =
              deploy-rs.lib.aarch64-linux.activate.nixos
              self.nixosConfigurations.pallas;
          };
        };
      };
    }
    # x86_64-linux-specific packages
    # TODO custom packages should be added to pkgs, made available globally
    # right now, my custom aarch64 installer can only be built on x86_64 linux
    // {
      packages.x86_64-linux = let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
        roc-rk3328-cc-bootloader = import ./nix/pkgs/roc-rk3328-cc-bootloader {
          inherit pkgs;
          lib = pkgs.lib;
        };

        # run with:
        # nix build .#arm-installer --system aarch64-linux
        arm-installer = nixos-generators.nixosGenerate {
          system = "aarch64-linux";
          modules = [
            ./nix/installers/aarch64-linux/configuration.nix
          ];
          specialArgs = {
            inherit inputs;
          };
          customFormats.aarch64-linux-roc = import ./nix/formats/aarch64-linux-roc/configuration.nix {
            inherit pkgs;
            bootloader = self.packages.x86_64-linux.roc-rk3328-cc-bootloader;
          };
          format = "aarch64-linux-roc";
        };
      };
    }
    # cross-platform applications and packages
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
    in {
      formatter = pkgs.alejandra;

      # I re-export deploy-rs due to an issue with running `nix flake github:serokell/deploy-rs ...`
      # per a conversation I had here: https://github.com/serokell/deploy-rs/issues/155
      apps.deploy-rs = deploy-rs.defaultApp."${system}";

      devShells.deploy-rs = pkgs.mkShell {
        buildInputs = [deploy-rs.defaultPackage."${system}"];
      };

      devShells.luaDev = pkgs.mkShell {
        buildInputs = with pkgs; [
          lua-language-server
          luajitPackages.luasocket
          stylua
        ];
      };
    });
}
