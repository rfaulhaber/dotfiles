{
  description = "My Nix system configurations.";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

  outputs = inputs@{ self, nixpkgs, ... }: {
    nixosConfigurations = {
      mir3 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./nix/hosts/mir3/configuration.nix ];
      };
    };
  };
}
