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
    # flake-parts.url = "github:hercules-ci/flake-parts";
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
    ...
  }: let
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
        pallas = mkHost ./nix/hosts/pallas/configuration.nix {
          system = "aarch64-linux";
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
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    }
    # x86_64-linux-specific packages
    # TODO custom packages should be added to pkgs, made available globally
    # right now, my custom aarch64 installer can only be built on x86_64 linux
    // {
      packages.x86_64-linux = let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
        roc-rk3328-cc-bootloader = import ./nix/pkgs/roc-rk3328-cc-bootloader {
          inherit pkgs;
          lib = pkgs.lib;
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

        # arm-installer-rpi5 = nixos-generators.nixosGenerate {
        #   system = "aarch64-linux";
        #   modules = [
        #     ./nix/installers/aarch64-linux/configuration.nix
        #   ];
        #   specialArgs = {
        #     inherit inputs;
        #   };
        #   customFormats.aarch64-linux-rpi5 = import ./nix/formats/aarch64/linux/raspberry-pi/5/configuration.nix {
        #     inherit pkgs inputs;
        #   };
        #   format = "aarch64-linux-rpi5";
        # };

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
    }
    // (let
      supportedSystems = ["x86_64-linux" "aarch64-darwin"];
      forSystems = systems: f:
        nixpkgs.lib.genAttrs systems
        (system:
          f system (import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          }));
      forAllSystems = forSystems supportedSystems;
    in {
      formatter = forAllSystems (system: pkgs: pkgs.alejandra);
      apps = forAllSystems (system: pkgs: {
        # I re-export deploy-rs due to an issue with running `nix flake github:serokell/deploy-rs ...`
        # per a conversation I had here: https://github.com/serokell/deploy-rs/issues/155
        deploy-rs = deploy-rs.defaultApp."${system}";
        generate = nixos-generators.apps.${system}.default;
      });
      devShells = forAllSystems (system: pkgs: {
        deploy-rs = pkgs.mkShell {
          buildInputs = [deploy-rs.defaultPackage."${system}"];
        };

        luaDev = pkgs.mkShell {
          buildInputs = with pkgs; [
            lua-language-server
            luajitPackages.fennel
            luajitPackages.luasocket
            stylua
            fnlfmt
          ];
        };

        cluster = pkgs.mkShell {
          buildInputs = with pkgs; [
            ansible
            terraform
            vagrant
          ];
        };

        generate = pkgs.mkShell {
          buildInputs = [
            nixos-generators.packages.${system}.default
          ];
        };

        default = self.devShells.${system}.generate;
      });
    });
}
