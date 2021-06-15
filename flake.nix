{
  description = "My Nix system configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      lib = nixpkgs.lib.extend (self: super: {
        my = import ./nix/lib {
          inherit inputs;
          lib = self;
        };
      });
    in {
      lib = lib.my;
      nixosConfigurations = {
        mir3 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ ./nix/hosts/mir3/configuration.nix ];
        };
      };
    };
}
