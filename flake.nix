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

    # nixpkgs' claude-code is a vendored binary manifest that lags upstream by
    # several releases. This flake's CI tracks the `latest` channel daily, so a
    # routine `nix flake update` pulls the newest Claude Code for free.
    claude-code = {
      url = "github:sadjow/claude-code-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
    niri-flake = {
      url = "github:epireyn/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    waybar = {
      url = "github:Alexays/waybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    noctalia = {
      url = "github:noctalia-dev/noctalia";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Independent project from noctalia proper: its own compositor and greetd
    # client, sharing only a palette file convention with the shell.
    noctalia-greeter = {
      url = "github:noctalia-dev/noctalia-greeter";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # I use flake-parts to ensure I can use my flake across platforms, although I probably shouldn't
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi";
    # so that we can use the pipeline operator lol

    nil.url = "github:oxalica/nil/main";

    # determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
    # temprorarily pinning determinate nix so that aarch64-linux hosts can build
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/=3.21.8";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rz.url = "github:rfaulhaber/rz";
    ghostel = {
      url = "github:rfaulhaber/ghostel-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    deploy-rs,
    flake-parts,
    nixos-raspberrypi,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} ({...}: {
      imports = [inputs.git-hooks.flakeModule];
      flake = let
        lib = inputs.nixpkgs.lib.extend (self: _super: {
          my = import ./nix/lib {
            inherit inputs;
            lib = self;
          };
        });
      in {
        # overlays = mapModules ./nix/overlays import;
        # these are the actual system configurations
        # any particular system can be build with nixos-rebuild of course, but also:
        # nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel
        # TODO utilize top-level nixosModules
        # TODO a nixos configuration should be a combination of a modules configuration and a hardware configuration
        nixosConfigurations = let
          mkHost = lib.my.mkNixOSHost;
        in {
          hyperion =
            mkHost ./nix/hosts/hyperion/configuration.nix {
            };
          atlas = mkHost ./nix/hosts/atlas/configuration.nix {};
          janus = mkHost ./nix/hosts/janus/configuration.nix {};
          pallas = mkHost ./nix/hosts/pallas/configuration.nix {};
          hecate = mkHost ./nix/hosts/hecate/configuration.nix {};
          vulcan = mkHost ./nix/hosts/vulcan/configuration.nix {};
          # prometheus uses the stock nixpkgs-unstable channel (via mkHost) and
          # pulls the Pi 5 kernel/firmware/overlays from nixos-raspberrypi. Going
          # through nixos-raspberrypi.lib.nixosSystem would re-pin nixpkgs to the
          # flake's 25.11 and break modules written against unstable (e.g. nix-cache).
          prometheus = mkHost ./nix/hosts/prometheus/configuration.nix {
            specialArgs = {
              inherit (inputs) nixos-raspberrypi;
            };
          };
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
            hecate = {
              hostname = "hecate";
              profiles.system = {
                user = "root";
                fastConnection = true;
                path =
                  deploy-rs.lib.aarch64-linux.activate.nixos
                  self.nixosConfigurations.hecate;
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
            vulcan = {
              hostname = "vulcan";
              profiles.system = {
                user = "root";
                path =
                  deploy-rs.lib.x86_64-linux.activate.nixos
                  self.nixosConfigurations.vulcan;
              };
            };
            prometheus = {
              hostname = "prometheus";
              profiles.system = {
                user = "root";
                fastConnection = true;
                path =
                  deploy-rs.lib.aarch64-linux.activate.nixos
                  self.nixosConfigurations.prometheus;
              };
            };
          };
        };

        # deploy-rs's deployChecks embeds EVERY node's toplevel as a build
        # input regardless of the check's own system — the ${system} only
        # picks which platform builds the check derivation. Handing it the
        # unfiltered deploy spec would make `nix flake check` cross-build the
        # other architecture's hosts (and on eos, every Linux host), so each
        # system gets a copy of the spec scoped to its own nodes. The systems
        # are listed literally rather than taken from top.config.systems:
        # aarch64-darwin has no deploy nodes and must not get a checks entry.
        checks = lib.genAttrs ["x86_64-linux" "aarch64-linux"] (
          system:
            deploy-rs.lib.${system}.deployChecks (self.deploy
              // {
                nodes =
                  lib.filterAttrs
                  (_: node: node.profiles.system.path.system == system)
                  self.deploy.nodes;
              })
        );

        packages.x86_64-linux = {
          rpi3-installer =
            (inputs.nixpkgs.lib.nixosSystem {
              system = "aarch64-linux";
              modules = [./nix/images/rpi3-installer.nix];
              specialArgs = {inherit inputs;};
            }).config.system.build.sdImage;
          rpi5-installer =
            (inputs.nixpkgs.lib.nixosSystem {
              system = "aarch64-linux";
              modules = [./nix/images/rpi5-installer.nix];
              specialArgs = {
                inherit inputs;
                inherit (inputs) nixos-raspberrypi;
              };
            }).config.system.build.sdImage;
          x86_64-installer =
            (inputs.nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = [./nix/images/x86_64-installer.nix];
            }).config.system.build.isoImage;
          # Flashable SD image of the prometheus host. Same NixOS configuration
          # as `nixosConfigurations.prometheus`, so deploy-rs can switch the
          # running system to a freshly built toplevel afterwards.
          prometheus-sd-image = self.nixosConfigurations.prometheus.config.system.build.sdImage;
        };
      };
      systems = ["x86_64-linux" "aarch64-darwin"];
      perSystem = {
        config,
        pkgs,
        inputs',
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
        packages.generated-configs = import ./nix/generated {
          inherit pkgs inputs;
          inherit (pkgs) lib;
        };

        pre-commit.settings.hooks = {
          alejandra.enable = true;
          deadnix.enable = true;
          statix.enable = true;
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs;
            [
              inputs'.deploy-rs.packages.default
              inputs'.nil.packages.default
              inputs'.sops-nix.packages.default
              dix
              rage
              skopeo
              sops
            ]
            ++ [
              # for running hooks by hand (`pre-commit run --all-files`);
              # the installed git hook pins its own store path and doesn't need this
              config.pre-commit.settings.package
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
              inputs'.nix-darwin.packages.default
            ];
          shellHook = config.pre-commit.installationScript;
        };
      };
    });
}
