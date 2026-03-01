" Minimal vimrc — fallback for machines without neovim
" Full config lives in ~/.config/nvim/init.lua

set nocompatible
let mapleader = ","
syntax on
filetype plugin indent on

set laststatus=2 clipboard=unnamed scrolloff=3
set number ruler cursorline nowrap
set expandtab tabstop=2 shiftwidth=2 softtabstop=2 autoindent
set hlsearch incsearch ignorecase smartcase
set splitright splitbelow
set autoread hidden
set noeb vb t_vb=
set backspace=indent,eol,start
set wildmode=list:longest,list:full

nnoremap <Leader><space> :noh<CR>
vnoremap < <gv
vnoremap > >gv
nnoremap j gj
nnoremap k gk
