set nocompatible
let mapleader = ","

" don't redraw while executing macros
set lazyredraw

" allow to have hidden buffers not written
set hidden

" time to wait after ESC (default causes annoying delay)
set timeoutlen=250

" enable syntax highlighting
syntax on
set number
set ruler

" indentation and tabs
set expandtab
set tabstop=3
set shiftwidth=3
set autoindent
set listchars=tab:▸\ ,eol:¬
noremap <silent><Leader>s :set list!<CR>

" tab completion
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" configure incremental search
set hlsearch
set incsearch
set ignorecase
set smartcase
noremap <silent><Leader><space> :noh<CR>

" window shortcuts
map <Leader>= <C-w>=
map <Leader>l <C-w>L
map <Leader>p <C-w>J

" opens an edit command with the path of the currently edited file filled in
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
" opens a vsplit edit command with the path of the currently edited file filled in
map <Leader>te :vsplit <C-R>=expand("%:p:h") . "/" <CR>

" _zsh* files are Zsh scripts
au BufNewFile,BufRead _zsh* set ft=zsh

" JSON files are Javascript
au BufNewFile,BufRead *.json set ft=javascript

" Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby

" make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

" make uses real tabs
au FileType make set noexpandtab

" default file encoding
set encoding=utf-8
set fileencoding=utf-8

" directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" load pathogen
source ~/.vim/bundle/pathogen/autoload/pathogen.vim

" pathogen plugin, requires filetype plugin indent
filetype plugin indent on
call pathogen#infect()
" required to get help on stuff installed through pathogen
call pathogen#helptags()

" default colorscheme
colorscheme molokai

" bubble single lines
nmap <C-Left> [e
nmap <C-Right> ]e
" bubble multiple lines
vmap <C-Left> [egv
vmap <C-Right> ]egv

" customize statuline
set statusline=%f%m\ %{fugitive#statusline()}\ %y\ %{SyntasticStatuslineFlag()}%=[POS=%l,%v][\ %{strftime(\"%H:%M:%S\")}\ ]

" highlight statusline when in INSERT mode
set laststatus=2
hi StatusLine ctermfg=darkgreen
hi StatusLineNC cterm=none 
function! InsertStatuslineColor(mode)
   if a:mode == 'i'
      hi StatusLine term=reverse ctermfg=darkred
   elseif a:mode == 'r'
      hi StatusLine term=reverse ctermfg=darkmagenta
   else
      hi StatusLine term=reverse ctermfg=darkblue
   endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi StatusLine term=reverse ctermfg=darkgreen

" map NERDTree to ,n
let g:NERDTreeWinSize=45
noremap <Leader>n :NERDTreeToggle<CR>

" ZoomWin configuration
map <silent><Leader><Leader> :ZoomWin<CR>

" Syntastic configuration
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_enable_signs=1
" errors split closes when no errors left
let g:syntastic_auto_loc_list=2
