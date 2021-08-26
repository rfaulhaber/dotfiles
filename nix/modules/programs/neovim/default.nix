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
        coc-fzf
        editorconfig-vim
        fzf-vim
        syntastic
        vim-airline
        vim-airline-themes
        vim-easymotion
        vim-gitgutter
        vim-surround
        vim-vinegar
        (mkIf (config.modules.langs.rust.enable) coc-rust-analyzer)
        (mkIf (config.modules.langs.rust.enable) rust-vim)
      ];

      extraConfig = ''
        nnoremap <SPACE> <Nop>
        let mapleader=" "
      '';

      vimAlias = true;
      viAlias = true;
      vimdiffAlias = true;
    };
  };
}
