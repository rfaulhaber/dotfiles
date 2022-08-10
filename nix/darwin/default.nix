{ config, lib, pkgs, ... }:

{
  config = {
    user = rec {
      name = "ryan";
      home = "/Users/${name}";
    };
  };
}
