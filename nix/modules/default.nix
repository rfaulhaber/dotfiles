{
  lib,
  isLinux,
  isDarwin,
  ...
}: {
  imports =
    [
      # Cross-platform modules
      ./dotfiles.nix
      ./globals.nix
      ./options.nix
      ./programs
      ./services
      ./themes
    ]
    ++ lib.optionals isLinux [
      # Linux-specific modules
      ./desktop
      ./hardware
      ./linux.nix
      ./system.nix
      ./xdg.nix
    ]
    ++ lib.optionals isDarwin [
      # Darwin-specific modules
      ./darwin.nix
    ];
}
