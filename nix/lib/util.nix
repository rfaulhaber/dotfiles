{
  lib,
  pkgs,
  ...
}:
with builtins;
with lib; {
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

  sum = builtins.foldl' (x: y: x + y) 0;

  count = pred:
    builtins.foldl' (acc: val:
      if (pred val)
      then acc + 1
      else acc)
    0;

  # thank you hlissner
  # mapFilterAttrs ::
  #   (name -> value -> bool)
  #   (name -> value -> { name = any; value = any; })
  #   attrs
  mapFilterAttrs = pred: f: attrs: filterAttrs pred (mapAttrs' f attrs);
}
