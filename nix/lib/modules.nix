# thank you hlissner
# https://github.com/hlissner/dotfiles/blob/089f1a9da9018df9e5fc200c2d7bef70f4546026/lib/modules.nix
{
  self,
  lib,
  ...
}:
with builtins;
with lib; let
  inherit (self.attrs) mapFilterAttrs;
in {
  # thank you hlissner
  mapModules = dir: fn:
    mapFilterAttrs (n: v: v != null && !(hasPrefix "_" n)) (n: v: let
      path = "${toString dir}/${n}";
    in
      if v == "directory" && pathExists "${path}/default.nix"
      then nameValuePair n (fn path)
      else if v == "regular" && n != "default.nix" && hasSuffix ".nix" n
      then nameValuePair (removeSuffix ".nix" n) (fn path)
      else nameValuePair "" null) (readDir dir);
}
