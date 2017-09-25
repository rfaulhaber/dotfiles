" vim config

" alias save as sudo to w!!
cmap w!! w !sudo tee > /dev/null %

" set line numbers
set number
set relativenumber

" set highlighting
set hlsearch
set incsearch

" ignore case while searching
set ignorecase

" replace tabs with four spaces
filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set autoindent
set smartindent

" split configuration
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
set splitbelow
set splitright

" vim-plug plugins
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'pangloss/vim-javascript'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'nanotech/jellybeans.vim'
Plug 'scrooloose/syntastic'
Plug 'kien/ctrlp.vim'
Plug 'tomlion/vim-solidity'

call plug#end()

" plugin config

" airline config
set laststatus=2
set ttimeoutlen=50

" jellybean config
colo jellybeans
set t_Co=256
let g:jellybeans_overrides = {
			\    'background': { 'guibg': '000000' },
			\}

" syntastic config
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_javascript_checkers = ['eslint']
