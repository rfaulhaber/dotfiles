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

    ragenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wezterm.url = "github:wez/wezterm?dir=nix";

    # TODO remove pin when no longer broken
    swww.url = "github:LGFae/swww?rev=b9aaba38c79e9915c62328861def7353f53dcdbd";
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
    # TODO remove pin when no longer broken
    waybar.url = "github:Alexays/waybar?rev=9d37dedb57922cb5cb68e06d9ae2ce6170d58b16";
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
    nixos-generators,
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
            pkgs = inputs.nixpkgs;
            lib = self;
          };
        });
      in {
        templates = {
          rust = {
            path = ./nix/templates/rust;
            description = "Rust project template";
          };
          rust-overlay = {
            path = ./nix/templates/rust-overlay;
            description = "rust-overlay project template";
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
            system = "x86_64-linux";
          };
          atlas = mkHost ./nix/hosts/atlas/configuration.nix {
            system = "x86_64-linux";
          };
          janus = mkHost ./nix/hosts/janus/configuration.nix {
            system = "x86_64-linux";
          };
          pallas = mkHost ./nix/hosts/pallas/configuration.nix {
            system = "aarch64-linux";
          };
          # NOTE: this will not compile due to this issue:
          # https://github.com/nvmd/nixos-raspberrypi/issues/51
          # nike = lib.my.mkRaspberryPiNixOSHost ./nix/hosts/nike/configuration.nix {
          #   system = "aarch64-linux";
          # };
        };
        darwinConfigurations = {
          eos = lib.my.mkDarwinHost ./nix/hosts/eos/configuration.nix {
            system = "aarch64-darwin";
          };
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
            # nike = {
            #   hostname = "nike";
            #   profiles.system = {
            #     user = "root";
            #     fastConnection = true;
            #     path =
            #       deploy-rs.lib.aarch64-linux.activate.nixos
            #       self.nixosConfigurations.nike;
            #   };
            # };
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

          # run with:
          # nix build .#arm-roc-installer
          arm-roc-installer = nixos-generators.nixosGenerate {
            system = "aarch64-linux";
            modules = [
              ./nix/installers/aarch64-linux/configuration.nix
            ];
            specialArgs = {
              inherit inputs;
            };
            customFormats.aarch64-linux-roc = import ./nix/formats/aarch64/linux/renegade-roc/configuration.nix {
              inherit pkgs;
              bootloader = self.packages.x86_64-linux.roc-rk3328-cc-bootloader;
            };
            format = "aarch64-linux-roc";
          };

          # supports raspberry pi up to version 4
          arm-installer-generic = nixos-generators.nixosGenerate {
            system = "aarch64-linux";
            modules = [
              ./nix/installers/aarch64-linux/configuration.nix
            ];
            specialArgs = {
              inherit inputs;
            };
            format = "sd-aarch64-installer";
          };

          x86_64-installer-generic = nixos-generators.nixosGenerate {
            system = "x86_64-linux";
            modules = [
              ./nix/installers/x86_64-linux/configuration.nix
            ];
            specialArgs = {
              inherit inputs;
            };
            format = "install-iso";
          };
        };
      };
      systems = ["x86_64-linux" "aarch64-darwin"];
      perSystem = {
        config,
        pkgs,
        inputs',
        self',
        ...
      }: {
        formatter = pkgs.alejandra;
        apps = {
          # I re-export deploy-rs due to an issue with running `nix flake github:serokell/deploy-rs ...`
          # per a conversation I had here: https://github.com/serokell/deploy-rs/issues/155
          deploy-rs = inputs'.deploy-rs.apps.default;
          generate = inputs'.nixos-generators.apps.default;
        };
        devShells.default = pkgs.mkShell {
          buildInputs =
            [
              inputs'.nixos-generators.packages.default
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
