{
  inputs,
  lib,
  ...
}:
with builtins;
with lib; let
  inherit (inputs) nixpkgs home-manager;
in rec {
  mkPkgs = system: pkgs: extraOverlays:
    import pkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = extraOverlays ++ (lib.attrValues inputs.self.overlays);
    };

  # thank you hlissner
  # https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix#L7
  mkHost = path: attrs @ {system, ...}: let
    pkgs = mkPkgs system nixpkgs [];
  in {
    inherit system;
    modules = [
      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
      }
      {
        nixpkgs.pkgs = pkgs;
        networking.hostName = hostnameFromPath path;
      }
      (filterAttrs (n: v: !elem n ["system"]) attrs)
      path
    ];
    specialArgs = {
      inherit lib inputs system;
      platform = system;
    };
  };

  mkNixOSHost = path: attrs:
    nixosSystem (mkHost path attrs);

  mkDarwinHost = path: attrs:
    darwinSystem (mkHost path attrs);

  # thank you hlissner
  mapHosts = dir: attrs @ {system ? defaultSystem, ...}:
    mapModules dir (hostPath: mkHost hostPath attrs);

  hostnameFromPath = path: head (match ".*/([[:alpha:]]+)/configuration.nix" (toString path));
}
