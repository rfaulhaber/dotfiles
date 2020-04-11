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

" plugins
call plug#begin(stdpath('data') . '/plugged')
Plug 'tpope/vim-surround'
Plug 'scrooloose/syntastic'
Plug 'vim-airline/vim-airline'
Plug 'rust-lang/rust.vim'
call plug#end()
