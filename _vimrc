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
noremap <silent><Leader>s :set list!<CR>

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" bubble single lines
nmap <C-Left> [e
nmap <C-Right> ]e
" bubble multiple lines
vmap <C-Left> [egv
vmap <C-Right> ]egv

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

" default file encoding
set encoding=utf-8
set fileencoding=utf-8

" tab completion
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*

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
let g:syntastic_auto_loc_list=2

" Opens an edit command with the path of the currently edited file filled in
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
" Opens a tab edit command with the path of the currently edited file filled in
map <Leader>te :vsplit <C-R>=expand("%:p:h") . "/" <CR>

" add json syntax highlighting
au BufNewFile,BufRead *.json set ft=javascript

" Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby

" make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

" make uses real tabs
au FileType make set noexpandtab

" directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backup

