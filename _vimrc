" Minimal vimrc — fallback for machines without neovim
" Full config lives in ~/.config/nvim/init.lua

" General settings {{{

set nocompatible
let mapleader = ","
let maplocalleader = ","

set laststatus=2
set backupdir=~/.vim/backup
set directory=~/.vim/backup
set clipboard=unnamed
set scrolloff=3
set undofile
set undodir=~/.vim/vimundo
set noeb vb t_vb=
set showcmd
set encoding=utf-8
set t_Co=256
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set hidden
set number
set ruler
set cursorline
set nowrap
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set autoindent
set listchars=tab:»\ ,eol:¬,extends:❯,precedes:❮
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*
set backspace=indent,eol,start
set hlsearch
set incsearch
set ignorecase
set smartcase
set splitright
set splitbelow
set colorcolumn=120
set shortmess+=A
set autoread
set breakindent
set viminfo='1024
set termguicolors
set background=dark
set diffopt+=iwhite

syntax on
filetype plugin indent on
au FileType vim setlocal foldmethod=marker

" }}}

" Keybindings {{{

nnoremap <Leader>; ,
noremap <silent><Leader>s :set list!<CR>
noremap <silent><Leader><space> :noh<CR>
nnoremap * *<c-o>
vnoremap * y/<c-r>"<cr><c-o>
nnoremap K <nop>
vnoremap < <gv
vnoremap > >gv
nnoremap p ]p`]
vnoremap p ]p`]
vnoremap y y`]
nnoremap P ]P
nmap do do]c
nmap dp dp]c
map Y y$
nnoremap j gj
nnoremap k gk
nnoremap n nzzzv
nnoremap N Nzzzv
tnoremap <Esc> <C-\><C-n>

inoremap <c-a> <esc>I
inoremap <c-e> <esc>A
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
nnoremap <Space> za
vnoremap <Space> za
nnoremap zJ zjzmza
nnoremap zK zkzmza[z
nmap <silent>= :res<CR>:vertical res<CR>
nmap <Leader>= <C-w>=
nmap <Leader>l <C-w>L
nmap <Leader>p <C-w>J
noremap <silent><Leader>d ]czz
noremap <silent><Leader>D [czz
noremap <silent><Leader>q :x<CR><C-w>j
cmap w!! w !sudo tee % > /dev/null <CR>
vnoremap <Leader>x :!recode utf8..html<CR>
nmap <silent><Leader>t :%!column -t<CR>
nnoremap <silent><Leader>V :tabnew<CR>:put a<CR>:diffthis<CR>:vnew<CR>:put b<CR>:diffthis<CR>
nnoremap <silent><Leader>Q :windo bd!<CR>tabclose<CR>
map ,c gc

" }}}
