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
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    deploy-rs,
    nixos-hardware,
    flake-utils,
    ...
  }: let
    # inherit (lib.my) mkHost;
    lib = nixpkgs.lib.extend (self: super: {
      my = import ./nix/lib {
        inherit inputs;
        pkgs = nixpkgs;
        lib = self;
      };
    });

    mkHost = cfgFile:
      nixpkgs.lib.nixosSystem rec {
        # TODO account for darwin
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
          cfgFile
        ];
        specialArgs = {
          inherit lib inputs;
          platform = system;
        };
      };
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

      # these are the actual system configurations
      nixosConfigurations = {
        hyperion = mkHost ./nix/hosts/hyperion/configuration.nix;
        atlas = mkHost ./nix/hosts/atlas/configuration.nix;
        helios = mkHost ./nix/hosts/helios/configuration.nix;
      };

      # TODO write a mapHosts function, like here: https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix
      # nixosConfigurations = mapHosts ./nix/hosts { };

      # run with: nix run '.#deploy-rs' '.#atlas'
      deploy.nodes.atlas = {
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
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # I re-export deploy-rs due to an issue with running `nix flake github:serokell/deploy-rs ...`
      # per a conversation I had here: https://github.com/serokell/deploy-rs/issues/155
      apps.deploy-rs = deploy-rs.defaultApp."${system}";

      devShells.default = pkgs.mkShell {
        buildInputs = [deploy-rs.defaultPackage."${system}"];
      };
    });
}
