# thank you hlissner
# https://github.com/hlissner/dotfiles/blob/089f1a9da9018df9e5fc200c2d7bef70f4546026/lib/modules.nix
{
  self,
  lib,
  ...
}: let
  inherit (builtins) readDir pathExists toString;
  inherit (lib) nameValuePair hasSuffix removeSuffix filterAttrs;
  inherit (lib.strings) hasPrefix;
  inherit (self.attrs) mapFilterAttrs;
in {
  # thank you hlissner
  mapModules = dir: fn: let
    pred = n: v: v != null && !(hasPrefix "_" n);
    f = n: v: let
      path = "${toString dir}/${n}";
    in
      if v == "directory" && pathExists "${path}/default.nix"
      then path |> fn |> nameValuePair
      else if v == "regular" && n != "default.nix" && hasSuffix ".nix" n
      then
        path
        |> fn
        |> (nameValuePair (removeSuffix ".nix" n))
      else nameValuePair "" null;
  in
    dir
    |> readDir
    |> filterAttrs pred
    |> (mapFilterAttrs pred f);
}
