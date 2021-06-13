{
  description = "My Nix system configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = inputs@{ self, nixpkgs, ... }: {
    nixosConfigurations = {
      mir3 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./nix/hosts/mir3/configuration.nix ];
      };
    };
  };
}
