{
  lib,
  pkgs,
  ...
}:
with builtins;
with lib; rec {
  mkOpt = type: default: mkOption {inherit type default;};

  mkOptDesc = type: default: description:
    mkOption {inherit type default description;};

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
      example = ["a" "b" "c"];
    };

  mkPath = path:
    if path != null
    then toString path
    else "";

  mkAssert = assertion: message: {inherit assertion message;};

  mkEnableSubmodule = {
    default,
    description ? "Enables this option",
  }:
    types.submodule {
      options = {
        enable = mkOption {
          inherit default description;
          type = types.bool;
        };
      };
    };

  attrsOfMkEnableSubmodule = args: types.attrsOf (mkEnableSubmodule args);

  sum = builtins.foldl' (x: y: x + y) 0;

  count = pred:
    builtins.foldl' (acc: val:
      if (pred val)
      then acc + 1
      else acc)
    0;

  # compose :: [functions] -> a -> <return type of last of functions>
  compose = flip pipe;

  # thank you hlissner
  # mapFilterAttrs ::
  #   (name -> value -> bool)
  #   (name -> value -> { name = any; value = any; })
  #   attrs
  mapFilterAttrs = pred: f: attrs: pipe attrs [(mapAttrs' f) (filterAttrs pred)];
}
