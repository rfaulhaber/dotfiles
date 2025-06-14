{
  lib,
  isLinux,
  isDarwin,
  ...
}: {
  imports =
    [
      ./1password
      ./age
      ./aspell
      ./direnv
      ./emacs
      ./eza
      ./ghostty
      ./git
      ./keychain
      ./kitty
      ./neovim
      ./nushell
      ./pcloud
      ./pijul
      ./starship
      ./wezterm
    ]
    ++ lib.optionals isLinux [
      # Linux-only programs
      ./zsh
    ];
}
