# this flake has gotten rather unwieldy and I need to refactor it severely
# a couple of improvements I need to make are:
# - refactor modules to not depend on pkgs
# - refactor configurations to be platform independent
# - refactor to allow the same host to be built for different targets, e.g. hyperion on a vm
# - allow the flake itself to be more command-based, e.g. "build a vm image of hyperion"
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
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # agenix is super busted. doesn't work for network deployments, and doesn't
    # work when trying to set a secret credential in /etc/fstab. Look into using
    # ragenix
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland/v0.47.2-b";

    wezterm.url = "github:wez/wezterm?dir=nix";

    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    swww.url = "github:LGFae/swww";
    # TODO consider using stylix
    # currently, a wallpaper is required with stylix. this is problematic
    # because I do not need to set a wallpaper in my config
    base16.url = "github:SenchoPens/base16.nix";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };
    murex.url = "github:rfaulhaber/murex";
    nix-doom-emacs-unstraightened = {
      url = "github:marienz/nix-doom-emacs-unstraightened";
      inputs.nixpkgs.follows = "";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
  flake-parts.lib.mkFlake { inherit inputs; } (top@{ config, withSystem, moduleWithSystem, ... }: {
    systems = [
      # systems for which you want to build the `perSystem` attributes
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ];
    imports = [
    ];
    flake = { config, inputs', ... }: {
      templates = import ./nix/templates;

      deploy = {
        sshUser = "ryan";
        autoRollback = true;
        magicRollback = true;
        nodes.atlas = import ./nix/hosts/atlas/deploy.nix {
          inherit (inputs') deploy-rs;
          inherit (config) nixosConfigurations;
      };
        nodes.pallas = import ./nix/hosts/pallas/deploy.nix {
          inherit (inputs') deploy-rs;
          inherit (config) nixosConfigurations;
      };
        };

      nixosConfigurations = {
        hyperion = mkHost ./nix/hosts/hyperion/configuration.nix {
          system = "x86_64-linux";
        };
        atlas = mkHost ./nix/hosts/atlas/configuration.nix {
          system = "x86_64-linux";
        };
        helios = mkHost ./nix/hosts/helios/configuration.nix {
          system = "x86_64-linux";
        };
        pallas = mkHost ./nix/hosts/pallas/configuration.nix {
          system = "aarch64-linux";
        };
        nike = mkHost ./nix/hosts/nike/configuration.nix {
          system = "aarch64-linux";
        };
        nexus = mkHost ./nix/hosts/nexus/configuration.nix {
          system = "x86_64-linux";
        };
      };

      };
    };
    perSystem = { config, pkgs, system, inputs', ... }: {
      formatter = pkgs.alejandra;

      apps = {
        deploy-rs =  inputs'.deploy-rs.apps.default;
        generate = inputs'.nixos-generators.apps.default;
      };



      # Recommended: move all package definitions here.
      # e.g. (assuming you have a nixpkgs input)
      # packages.foo = pkgs.callPackage ./foo/package.nix { };
      # packages.bar = pkgs.callPackage ./bar/package.nix {
      #   foo = config.packages.foo;
      # };
    };
  });

let
    inherit (lib.my) mapModules mkPkgs;

    pkgs = mkPkgs nixpkgs [];

    # TODO import lib functions that don't depend on pkgs
    lib = nixpkgs.lib.extend (self: super: {
      my = import ./nix/lib {
        inherit inputs pkgs;
        lib = self;
      };
    });
  in
    {

      # TODO utilize top-level nixosModules

      overlays = mapModules ./nix/overlays import;

      # these are the actual system configurations
      # any particular system can be build with nixos-rebuild of course, but also:
      # nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel

      # TODO refactor as packages, configurations can be built as packages
      # like so: packages.<platform>.nixosConfigurations.<hostname>.config.system.build.toplevel
      nixosConfigurations = let
        mkHost = lib.my.mkNixOSHost;
      in {
        hyperion = mkHost ./nix/hosts/hyperion/configuration.nix {
          system = "x86_64-linux";
        };
        atlas = mkHost ./nix/hosts/atlas/configuration.nix {
          system = "x86_64-linux";
        };
        helios = mkHost ./nix/hosts/helios/configuration.nix {
          system = "x86_64-linux";
        };
        pallas = mkHost ./nix/hosts/pallas/configuration.nix {
          system = "aarch64-linux";
        };
        nike = mkHost ./nix/hosts/nike/configuration.nix {
          system = "aarch64-linux";
        };
        nexus = mkHost ./nix/hosts/nexus/configuration.nix {
          system = "x86_64-linux";
        };
      };
      #// lib.my.mkNixOSK8sNodes 4 ./nix/hosts/nexus/configuration.nix { system = "x86_64-linux"; masterAddress = "10.0.0.1"; };

      # TODO add darwin configurations

      # TODO write a mapHosts function, like here: https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix
      # nixosConfigurations = mapHosts ./nix/hosts { };

      deploy = {
        sshUser = "ryan";
        autoRollback = true;
        magicRollback = true;
        nodes = {
          # run with: nix run '.#deploy-rs' '.#atlas'
          atlas = {
            hostname = "atlas";
            profiles.system = {
              user = "root";
              path =
                deploy-rs.lib.x86_64-linux.activate.nixos
                self.nixosConfigurations.atlas;
            };
          };
          pallas = {
            hostname = "pallas";
            profiles.system = {
              user = "root";
              fastConnection = true;
              path =
                deploy-rs.lib.aarch64-linux.activate.nixos
                self.nixosConfigurations.pallas;
            };
          };
          nike = {
            hostname = "nike";
            profiles.system = {
              user = "root";
              fastConnection = true;
              activationTimeout = 600;
              path =
                deploy-rs.lib.aarch64-linux.activate.nixos
                self.nixosConfigurations.nike;
            };
          };
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    }
}
