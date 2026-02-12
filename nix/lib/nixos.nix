{
  inputs,
  lib,
  ...
}: let
  inherit (builtins) head match toString dirOf;
  inherit (lib) strings;
in rec {
  # thank you hlissner
  # https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix#L7
  mkHost = path: attrs @ {
    system,
    overlays ? [],
    specialArgs ? {},
    extraModules ? [],
    ...
  }: let
    isLinux = strings.hasSuffix "linux" system;
    isDarwin = strings.hasSuffix "darwin" system;
    isAarch64 = strings.hasPrefix "aarch64" system;
    homeManagerModule =
      if isDarwin
      then inputs.home-manager.darwinModules.home-manager
      else inputs.home-manager.nixosModules.home-manager;
    hostname = hostnameFromPath path;
  in {
    inherit system;
    modules =
      [
        homeManagerModule
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
        {
          networking.hostName = hostname;
          nixpkgs.config.allowUnfree = true;
        }
        ({...}: {
          nixpkgs.overlays = overlays;
        })
        ../../nix/modules
        path
      ]
      ++ extraModules;
    specialArgs =
      {
        inherit inputs lib system isLinux isDarwin isAarch64 hostname;
        platform = system;
        hostDir = dirOf path;
      }
      // specialArgs;
  };

  mkNixOSHost = path: attrs:
    inputs.nixpkgs.lib.nixosSystem (mkHost path attrs);

  mkRaspberryPiNixOSHost = path: attrs:
    inputs.nixos-raspberrypi.lib.nixosSystem (mkHost path (attrs
      // {
        specialArgs.nixos-raspberrypi = inputs.nixos-raspberrypi;
      }));

  mkDarwinHost = path: attrs:
    inputs.nix-darwin.lib.darwinSystem (mkHost path attrs);

  hostnameFromPath = path: head (match ".*/([[:alpha:]]+)/configuration.nix" (toString path));
}
