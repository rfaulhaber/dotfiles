{ config, lib, pkgs, ... }:

{
  imports = [ ./docker-cleanup.nix ./updatedb.nix ./ssh-agent.nix ];
}
