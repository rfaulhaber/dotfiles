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
    in {
      lib = lib.my;
      nixosConfigurations = {
        mir3 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
            ./nix/hosts/mir3/configuration.nix
          ];
          extraArgs = { inherit inputs home-manager; };
          specialArgs = { inherit lib inputs; };
        };
      };
    };
}
