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
    flake-utils.url = "github:numtide/flake-utils";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    deploy-rs,
    nixos-hardware,
    flake-utils,
    ...
  }: let
    inherit (lib.my) mapModules;
    # does not support macOS yet!
    system = "x86_64-linux";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };

    pkgs = mkPkgs nixpkgs [];

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
        emacs-lisp = {
          path = ./nix/templates/emacs-lisp;
          description = "Emacs Lisp template";
        };
      };

      overlays =
        mapModules ./nix/overlays import;

      formatter = pkgs.alejandra;

      # these are the actual system configurations
      # any particular system can be build with nixos-rebuild of course, but also:
      # nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel
      nixosConfigurations = {
        hyperion = lib.my.mkHost ./nix/hosts/hyperion/configuration.nix {};
        atlas = lib.my.mkHost ./nix/hosts/atlas/configuration.nix {};
        helios = lib.my.mkHost ./nix/hosts/helios/configuration.nix {};
        cerberus = lib.my.mkHost ./nix/hosts/cerberus/configuration.nix {system = "aarch64-linux";};
      };

      # TODO write a mapHosts function, like here: https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix
      # nixosConfigurations = mapHosts ./nix/hosts { };

      # run with: nix run '.#deploy-rs' '.#atlas'
      deploy.nodes.atlas = {
        hostname = "atlas";
        sshUser = "ryan";
        sshOpts = ["-t"];
        autoRollback = true;
        magicRollback = false;
        profiles.system = {
          user = "root";
          path =
            deploy-rs.lib.x86_64-linux.activate.nixos
            self.nixosConfigurations.atlas;
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # I re-export deploy-rs due to an issue with running `nix flake github:serokell/deploy-rs ...`
      # per a conversation I had here: https://github.com/serokell/deploy-rs/issues/155
      apps.deploy-rs = deploy-rs.defaultApp."${system}";

      devShells.deploy-rs = pkgs.mkShell {
        buildInputs = [deploy-rs.defaultPackage."${system}"];
      };

      devShells.luaDev = pkgs.mkShell {
        buildInputs = with pkgs; [
          lua-language-server
          luajitPackages.luasocket
          stylua
        ];
      };
    });
}
