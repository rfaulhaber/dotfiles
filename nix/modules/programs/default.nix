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
      ./git
      ./kitty
      ./neovim
      ./nushell
      ./pijul
      ./wezterm
    ]
    ++ lib.optionals isLinux [
      # Linux-only programs
      ./zsh
    ];
}
