{
  description = "My Nix system configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager";
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
          extraArgs = { inherit inputs home-manager; };
          specialArgs = {
            inherit lib inputs;
            platform = system;
          };
        };
    in {
      lib = lib.my;
      nixosConfigurations = {
        mir3 = mkHost ./nix/hosts/mir3/configruation.nix;
        nil = mkHost ./nix/hosts/nil/configuration.nix;
      };
    };
}
