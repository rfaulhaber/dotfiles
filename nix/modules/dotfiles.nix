# Validates that config.dotfiles.dir resolves to a real tree. Modules that need
# repo content at runtime no longer symlink the whole repo into
# ~/.config/dotfiles; they import the specific subtree they need via
# builtins.path (see programs/nushell, niri/binds.nix) so the closure stays
# content-addressed and reproducible across flake fetch methods.
{config, ...}: {
  config = {
    assertions = [
      {
        assertion = builtins.pathExists config.dotfiles.dir;
        message = "config.dotfiles.dir does not exist";
      }
    ];
  };
}
