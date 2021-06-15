{
  description = "My Nix system configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    darwin.url = "github:lnl7/nix-darwin";
  };

  outputs = inputs@{ self, nixpkgs, darwin, ... }:
    let
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          config.allowUnfree = true;
          overlays = extraOverlays ++ (lib.attrValues self.overlays);
        };
      pkgs = mkPkgs nixpkgs [ self.overlay ];

      lib = nixpkgs.lib.extend (self: super: {
        my = import ./nix/lib {
          inherit pkgs inputs;
          lib = self;
        };
      });
    in {
      lib = lib.my;
      nixosConfigurations = {
        mir3 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            inputs.home-manager.nixosModules.home-manager
            (import ./nix)
            {
              desktop = {
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
