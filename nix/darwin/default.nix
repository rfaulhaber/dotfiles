{pkgs, ...}: {
  imports = [
    ./options.nix
    ../modules/dotfiles.nix
  ];

  config = {
    user.packages = with pkgs; [
    ];
  };
}
