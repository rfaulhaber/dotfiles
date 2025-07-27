{
  inputs,
  lib,
  ...
}:
with builtins;
with lib; rec {
  # thank you hlissner
  # https://github.com/hlissner/dotfiles/blob/master/lib/nixos.nix#L7
  mkHost = path: attrs @ {
    system,
    overlays ? [],
    specialArgs ? {},
    extraModules ? [],
    ...
  }: let
    isLinux = lib.strings.hasSuffix "linux" system;
    isDarwin = lib.strings.hasSuffix "darwin" system;
    homeManagerModule =
      if isDarwin
      then inputs.home-manager.darwinModules.home-manager
      else inputs.home-manager.nixosModules.home-manager;
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
          networking.hostName = hostnameFromPath path;
          nixpkgs.config.allowUnfree = true;
        }
        ../../nix/modules
        path
      ]
      ++ extraModules;
    specialArgs =
      {
        inherit lib inputs system isLinux isDarwin;
        platform = system;
        hostDir = dirOf path;
      }
      // specialArgs;
  };

  mkK8sNode = path: {
    system,
    overlays ? [],
    masterAddress,
    thisAddress,
    isMaster,
    hostname,
  }: (mkHost path {
    inherit system overlays;
    specialArgs = {
      inherit thisAddrss isMaster masterAddress;
    };
    extraModules = [
      {
        networking.hostName = lib.mkForce hostname;
      }
    ];
  });

  mkK8sNodes = count: path: {
    system,
    overlays ? [],
    masterAddress,
  }: let
    incrementIPAddress = n: address:
      lib.pipe address [
        (lib.splitString ".")
        (x: let
          first3 = lib.take 3 x;
          last = (lib.toInt (lib.last x)) + n;
        in
          first3 ++ [(builtins.toString last)])
        (builtins.concatStringsSep ".")
      ];
  in
    map (n:
      mkK8sNode path {
        inherit system overlays masterAddress;

        isMaster = n == 1;
        hostname = "${hostnameFromPath path}-${(
          if n == 1
          then "master"
          else n
        )}";
        thisAddress =
          if n == 1
          then masterAddress
          else (incrementIPAddress (n - 1) masterAddress);
      })
    (lib.range
      1
      count);

  mkNixOSHost = path: attrs:
    inputs.nixpkgs.lib.nixosSystem (mkHost path attrs);

  mkNixOSK8sNodes = n: path: attrs:
    map nixosSystem (mkK8sNodes n path attrs);

  mkDarwinHost = path: attrs:
    inputs.nix-darwin.lib.darwinSystem (mkHost path attrs);

  # thank you hlissner
  mapHosts = dir: attrs @ {system ? defaultSystem, ...}:
    mapModules dir (hostPath: mkHost hostPath attrs);

  hostnameFromPath = path: head (match ".*/([[:alpha:]]+)/configuration.nix" (toString path));
}
