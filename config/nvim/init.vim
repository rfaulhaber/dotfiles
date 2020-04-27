" plugins
call plug#begin(stdpath('data') . '/plugged')
Plug 'scrooloose/syntastic'
Plug 'rust-lang/rust.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rust-lang/rust.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-vinegar'
Plug 'chriskempson/base16-vim'
call plug#end()

" plugin config
let g:rust_recommended_style = 0
let g:airline_theme='base16'

" color scheme
colorscheme base16-default-dark
set termguicolors

" vim config

" alias save as sudo to w!!
cmap w!! w !sudo tee > /dev/null %

" set line numbers
set number

" set highlighting
set hlsearch
set incsearch

" ignore case while searching
set ignorecase

" split configuration
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
set splitbelow
set splitright

" indenting
set autoindent
set smartindent
set noexpandtab " Make sure that every file uses real tabs, not spaces
set shiftround  " Round indent to multiple of 'shiftwidth'
set ts=4

" misc
set audochdir

" Set the tab width
let s:tabwidth=4
exec 'set tabstop='    .s:tabwidth
exec 'set shiftwidth=' .s:tabwidth
