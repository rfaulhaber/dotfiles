{
  description = "My Nix system configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager";
    # deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      lib = nixpkgs.lib.extend (self: super: {
        my = import ./nix/lib/default.nix {
          inherit inputs;
          lib = self;
          pkgs = nixpkgs;
        };
      });
      mkHost = cfgFile:
        nixpkgs.lib.nixosSystem rec {
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
    in {
      lib = lib.my;
      # these are the actual system configurations
      nixosConfigurations = {
        mir3 = mkHost ./nix/hosts/mir3/configuration.nix;
        atlas = mkHost ./nix/hosts/atlas/configuration.nix;
      };

      # TODO utilize once deploy-rs fixes its SSH bugs
      # deploy.nodes.atlas = {
      #   hostname = "atlas";
      #   sshUser = "ryan";
      #   sshOpts = [ "-t" ];
      #   autoRollback = true;
      #   profiles.system = {
      #     user = "root";
      #     path = deploy-rs.lib.x86_64-linux.activate.nixos
      #       self.nixosConfigurations.atlas;
      #   };
      # };

      # # per the deploy-rs documentation
      # checks = builtins.mapAttrs
      #   (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
