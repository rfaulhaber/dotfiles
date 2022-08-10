# NOTE this configuration is temporary while the rest of my modules are adapted
# for cross-platform behavior.
# See https://daiderd.com/nix-darwin/manual/index.html
# for a complete list of darwin programs

{ config, pkgs, lib, inputs, home-manager, ... }:

{
  imports = [ ../../darwin ../../common ];

  # darwin = {
  #   # emacs = true;
  #   zsh = {
  #     enable = true;
  #     setDefault = true;
  #   };
  # };
  common = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
      };
    };
    themes.active = "moonlight";
  };

  environment.systemPackages = with pkgs; [
    aspell
    bat
    croc
    curl
    exa
    fd
    gnupg
    neovim
    nixfmt
    ripgrep
  ];

  services.nix-daemon.enable = true;
}
