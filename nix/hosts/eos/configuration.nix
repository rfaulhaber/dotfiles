# NOTE this configuration is temporary while the rest of my modules are adapted
# for cross-platform behavior.
# See https://daiderd.com/nix-darwin/manual/index.html
# for a complete list of darwin programs

{ config, pkgs, lib, inputs, home-manager, ... }:

{
  imports = [ ../../modules ../../platforms/darwin.nix ];

  modules = {
    # emacs = true;
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
}
