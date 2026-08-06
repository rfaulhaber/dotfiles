{
  inputs,
  lib,
  ...
}: let
  inherit (builtins) head match toString dirOf;
in rec {
  # thank you hlissner
  # https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix#L7
  mkHost = path: {
    overlays ? [],
    specialArgs ? {},
    extraModules ? [],
    ...
  }: let
    hostname = hostnameFromPath path;
  in {
    modules =
      [
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            backupFileExtension = "hm-backup";
          };
        }
        {
          networking.hostName = hostname;
          # this is kind of less than ideal, I feel like modules that use unfree
          # code should have to manually opt into this, but this is easier said
          # than done for some reason
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
        inherit inputs lib hostname;
        hostDir = dirOf path;
      }
      // specialArgs;
  };

  mkNixOSHost = path: attrs:
    inputs.nixpkgs.lib.nixosSystem (mkHost path (attrs
      // {
        extraModules =
          (attrs.extraModules or [])
          ++ [inputs.home-manager.nixosModules.home-manager];
        specialArgs =
          (attrs.specialArgs or {})
          // {
            isLinux = true;
            isDarwin = false;
          };
      }));

  mkDarwinHost = path: attrs:
    inputs.nix-darwin.lib.darwinSystem (mkHost path (attrs
      // {
        extraModules =
          (attrs.extraModules or [])
          ++ [inputs.home-manager.darwinModules.home-manager];
        specialArgs =
          (attrs.specialArgs or {})
          // {
            isLinux = false;
            isDarwin = true;
          };
      }));

  hostnameFromPath = path:
    toString path
    |> match ".*/([[:alnum:]-]+)/configuration.nix"
    |> head;
}
