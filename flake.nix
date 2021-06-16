{
  description = "My Nix system configurations.";

  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    darwin.url = "github:lnl7/nix-darwin";
  };

  outputs = inputs@{ self, nixpkgs, darwin, ... }: {
    nixosConfigurations = {
      mir3 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.home-manager.nixosModules.home-manager
          (import ./nix)
          {
            config.modules.desktop = {
              bspwm.enable = true;
              polybar.enable = true;
            };
          }
        ];
      };
    };
    darwinConfigurations.orange = darwin.lib.darwinSystem {
      modules = [ ./nix/hosts/orange/configuration.nix ];
    };
  };
}
