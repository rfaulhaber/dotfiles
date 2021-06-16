{
  description = "My Nix system configurations.";

  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    darwin.url = "github:lnl7/nix-darwin";
  };

  outputs = inputs@{ self, nixpkgs, darwin, home-manager, ... }: {
    nixosConfigurations = {
      mir3 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          (import ./nix)
          {
            modules.desktop = {
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
