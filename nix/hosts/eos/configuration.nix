# NOTE this configuration is temporary while the rest of my modules are adapted
# for cross-platform behavior.

{ config, pkgs, lib, inputs, home-manager, ... }:

{
  imports = [ ../../modules ../../platforms/darwin.nix ];

  modules = {
    emacs = true;
    zsh = {
      enable = true;
      setDefault = true;
    };
  };

  environment.systemPackages = with pkgs; [
    aspell
    bat
    croc
    curl
    emacs28NativeComp
    exa
    fd
    gnupg
    neovim
    nixfmt
    ripgrep
  ];

  services.nix-daemon.enable = true;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };

  users.users.ryan = {
    name = "ryan";
    home = "/Users/ryan";
  };
}
