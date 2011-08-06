set nocompatible
let mapleader = ","

" load pathogen
source ~/.vim/bundle/pathogen/autoload/pathogen.vim

" pathogen plugin, requires filetype plugin indent
filetype plugin indent on
call pathogen#infect()
" required to get help on stuff installed through pathogen
call pathogen#helptags()

" enable syntax highlighting
syntax on
set number
set ruler
colorscheme molokai

" indenting and tabs
set expandtab
set tabstop=3
set shiftwidth=3
set autoindent
set listchars=tab:▸\ ,eol:¬
noremap <Leader>l :set list!<CR>

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Bubble single lines
"nmap <M-Up> [e
"nmap <M-Down> ]e
" Bubble multiple lines
"vmap <M-Up> [egv
"vmap <M-Down> ]egv

" search
set hlsearch
set incsearch
set ignorecase
set smartcase
noremap <Leader><space> :noh<CR>

" window shortcuts (TODO)
map <Leader>= <C-w>=

" default file encoding
set encoding=utf-8
set fileencoding=utf-8

" tab completion
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*

" insert fugitive info in statusline
set statusline=%f%m\ %{fugitive#statusline()}\ %y\ [POS=%l,%v]\ %=[\ %{strftime(\"%H:%M:%S\")}\ ]

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
map <Leader><Leader> :ZoomWin<CR>

" Opens an edit command with the path of the currently edited file filled in
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
" Opens a tab edit command with the path of the currently edited file filled in
map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

" add json syntax highlighting
au BufNewFile,BufRead *.json set ft=javascript

" Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby

" make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

" make uses real tabs
au FileType make set noexpandtab

" Directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backup

