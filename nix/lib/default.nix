{ lib, ... }:

let inherit (lib) mkOption types;
in {
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
}
