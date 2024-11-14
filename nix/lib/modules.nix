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
  mapModules = dir: fn: let
    pred = n: v: v != null && !(hasPrefix "_" n);
    f = n: v: let
      path = "${toString dir}/${n}";
    in
      if v == "directory" && pathExists "${path}/default.nix"
      then pipe path [fn nameValuePair]
      else if v == "regular" && n != "default.nix" && hasSuffix ".nix" n
      then pipe path [fn (nameValuePair (removeSuffix ".nix" n))]
      else nameValuePair "" null;
  in
    pipe dir [readDir (mapFilterAttrs pred f)];
}
