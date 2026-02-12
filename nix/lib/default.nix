{
  inputs,
  lib,
  ...
}: let
  inherit (lib) mkOption;
  nixos = import ./nixos.nix {inherit inputs lib;};
in
  nixos
  // {
    mkOpt = type: default: mkOption {inherit type default;};

    mkOptDesc = type: default: description:
      mkOption {inherit type default description;};

    writeNushellScriptBin = pkgs: name: text:
      pkgs.writeScriptBin name ''
        #!${pkgs.nushell}/bin/nu

        ${text}
      '';
  }
