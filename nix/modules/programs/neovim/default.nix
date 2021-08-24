{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim = { enable = mkEnableOption false; };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ fzf ];

    home.programs.neovim = {
      enable = true;
      coc = { enable = true; };

      plugins = with pkgs.vimPlugins; [
        syntastic
        (mkIf (config.modules.langs.rust.enable) rust-vim)
        (mkIf (config.modules.langs.rust.enable) coc-rust-analyzer)
        editorconfig-vim
        vim-surround
        vim-airline
        vim-airline-themes
        coc-fzf
        fzf-vim
        vim-gitgutter
        vim-vinegar
      ];

      vimAlias = true;
      viAlias = true;
      vimdiffAlias = true;
    };
  };
}
