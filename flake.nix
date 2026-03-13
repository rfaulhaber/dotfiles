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
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    ragenix = {
      url = "github:yaxitech/ragenix";
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
    nix-doom-emacs-unstraightened = {
      url = "github:marienz/nix-doom-emacs-unstraightened";
      inputs.nixpkgs.follows = "";
    };
    niri = {
      url = "github:YaLTeR/niri";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # NOTE temporarily pin waybar, latest commit is broken on nix
    waybar.url = "github:Alexays/waybar";
    # I use flake-parts to ensure I can use my flake across platforms, although I probably shouldn't
    flake-parts.url = "github:hercules-ci/flake-parts";
    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi";
    # so that we can use the pipeline operator lol
    nil = {
      url = "github:oxalica/nil/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    deploy-rs,
    nixos-hardware,
    nix-darwin,
    emacs-overlay,
    flake-parts,
    nixos-raspberrypi,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} (top @ {
      config,
      withSystem,
      moduleWithSystem,
      system,
      ...
    }: {
      imports = [];
      flake = let
        lib = inputs.nixpkgs.lib.extend (self: super: {
          my = import ./nix/lib {
            inherit inputs;
            lib = self;
          };
        });
      in {
        templates = {
          rust = {
            path = ./nix/templates/rust;
            description = "Rust project template";
          };
          emacs-lisp = {
            path = ./nix/templates/emacs-lisp;
            description = "Emacs Lisp template";
          };
        };
        # overlays = mapModules ./nix/overlays import;
        # these are the actual system configurations
        # any particular system can be build with nixos-rebuild of course, but also:
        # nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel
        # TODO utilize top-level nixosModules
        # TODO a nixos configuration should be a combination of a modules configuration and a hardware configuration
        nixosConfigurations = let
          mkHost = lib.my.mkNixOSHost;
        in {
          hyperion = mkHost ./nix/hosts/hyperion/configuration.nix {
            overlays = [
              (import ./nix/overlays/netbird.nix)
            ];
          };
          atlas = mkHost ./nix/hosts/atlas/configuration.nix {};
          janus = mkHost ./nix/hosts/janus/configuration.nix {};
          pallas = mkHost ./nix/hosts/pallas/configuration.nix {};
        };
        darwinConfigurations = {
          eos = lib.my.mkDarwinHost ./nix/hosts/eos/configuration.nix {};
        };
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
            janus = {
              hostname = "janus";
              profiles.system = {
                user = "root";
                path =
                  deploy-rs.lib.x86_64-linux.activate.nixos
                  self.nixosConfigurations.janus;
              };
            };
          };
        };

        checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

        packages.x86_64-linux = let
          system = "x86_64-linux";
          pkgs = import inputs.nixpkgs {
            inherit system;
          };
          lib = pkgs.lib;
        in {
          roc-rk3328-cc-bootloader = import ./nix/pkgs/roc-rk3328-cc-bootloader {
            inherit pkgs lib;
          };
        };
      };
      systems = ["x86_64-linux" "aarch64-darwin"];
      perSystem = {
        config,
        pkgs,
        inputs',
        self',
        system,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
        };
        formatter = pkgs.alejandra;

        apps = {
          # I re-export deploy-rs due to an issue with running `nix flake github:serokell/deploy-rs ...`
          # per a conversation I had here: https://github.com/serokell/deploy-rs/issues/155
          deploy-rs = inputs'.deploy-rs.apps.default;
        };
        devShells.default = pkgs.mkShell {
          packages =
            [
              inputs'.nil.packages.default
              inputs'.deploy-rs.packages.default
              inputs'.sops-nix.packages.default
              pkgs.nvd
              pkgs.rage
              pkgs.sops
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
              inputs'.nix-darwin.packages.default
            ];
        };
      };
    });
}
