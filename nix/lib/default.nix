{ lib, inputs, ... }:

with builtins;
with lib;

let inherit (inputs) nixpkgs home-manager;
in rec {
  mkOpt = type: default: mkOption { inherit type default; };

  mkOptDesc = type: default: description:
    mkOption { inherit type default description; };

  mkBoolOpt = default:
    mkOption {
      inherit default;
      type = types.bool;
      example = true;
    };

  mkStringOpt = default:
    mkOption {
      inherit default;
      type = types.lines;
      example = "";
    };

  mkListOfStringOpt = default:
    mkOption {
      inherit default;
      type = types.listOf types.lines;
      example = [ "a" "b" "c" ];
    };

  mkPath = path: if path != null then toString path else "";

  mkAssert = assertion: message: { inherit assertion message; };

  mkHost = path:
    attrs@{ system, hardware }:
    nixpkgs.lib.nixosSystem rec {
      inherit system;
      modules = [
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
        {
          networking.hostName =
            mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (import path)
        hardware
      ];
      specialArgs = {
        inherit lib inputs system;
        platform = system;
      };
    };

  # thank you hlissner
  # mapFilterAttrs ::
  #   (name -> value -> bool)
  #   (name -> value -> { name = any; value = any; })
  #   attrs
  mapFilterAttrs = pred: f: attrs: filterAttrs pred (mapAttrs' f attrs);

  # thank you hlissner
  mapModules = dir: fn:
    mapFilterAttrs (n: v: v != null && !(hasPrefix "_" n)) (n: v:
      let path = "${toString dir}/${n}";
      in if v == "directory" && pathExists "${path}/default.nix" then
        nameValuePair n (fn path)
      else if v == "regular" && n != "default.nix" && hasSuffix ".nix" n then
        nameValuePair (removeSuffix ".nix" n) (fn path)
      else
        nameValuePair "" null) (readDir dir);

  # thank you hlissner
  mapHosts = dir:
    attrs@{ system ? system, ... }:
    mapModules dir (hostPath: mkHost hostPath attrs);
}
