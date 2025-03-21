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
    flake-parts.lib.mkFlake {inherit inputs;} (top @ {
      config,
      withSystem,
      moduleWithSystem,
      flake-parts-lib,
      ...
    }: let
      inherit (flake-parts-lib) importApply;
      flakeModules = {
        modules = import ./nix/modules/default.nix;
        hosts = importApply ./nix/hosts/default.nix {inherit withSystem;};
      };
    in {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      imports = [
      ];
      flake = {
        templates = import ./nix/templates;

        deploy = importApply ./nix/deploy/default.nix {inherit withSystem;};

        nixosModules = let mapModules = builtins.readDir ./nix/modules

        nixosModules.modules = moduleWithSystem (
          perSystem @ {
            config,
            pkgs,
            system,
            inputs',
          }: nixos @ {...}: {
            imports = [./nix/modules/default.nix];
          }
        );
      };
      perSystem = {
        config,
        pkgs,
        system,
        inputs',
        ...
      }: {
        formatter = pkgs.alejandra;

        apps = {
          deploy-rs = inputs'.deploy-rs.apps.default;
          generate = inputs'.nixos-generators.apps.default;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            inputs'.nixos-generators.packages.default
            # inputs'.deploy-rs.packages.default
          ];
        };
      };
    });
}
