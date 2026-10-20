{
  lib,
  isLinux,
  isDarwin,
  ...
}: {
  imports =
    [
      ./conf.nix
      ./dotfiles.nix
      ./emudhra_distrust_20261020.nix
      ./globals.nix
      ./gpg
      ./options.nix
      ./programs
      ./ssh
      ./themes
    ]
    ++ lib.optionals isLinux [
      ./linux
    ]
    ++ lib.optionals isDarwin [
      ./darwin
    ];
}
