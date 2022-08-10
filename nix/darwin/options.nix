{ config, lib, pkgs, ... }:

{
  imports = [ ../common/options.nix ];
  config = {
    user = rec {
      name = "ryan";
      home = "/Users/${name}";
    };
  };
}
