set nocompatible
let mapleader = ","
" remap crazy CTRL-]
nmap <Leader>$ <C-]>

" activate support for 256-color terminals
set t_Co=256

" don't redraw while executing macros
set lazyredraw

" allow to have hidden buffers not written
set hidden

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

" mappings for navigating the location list
map <Leader>J :lfirst<CR>
map <Leader>j :lnext<CR>
map <Leader>K :llast<CR>
map <Leader>k :lprevious<CR>

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

" use 4-space tabs in markdown
au FileType markdown set softtabstop=4 tabstop=4 shiftwidth=4

" use xmllint to format xml
au FileType xml set equalprg=xmllint\ --format\ -

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
hi StatusLine ctermfg=darkgrey
hi StatusLineNC cterm=none 
function! InsertStatuslineColor(mode)
   if a:mode == 'i'
      hi StatusLine term=reverse ctermfg=darkred
   endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi StatusLine term=reverse ctermfg=darkgrey

" configure FuzzyFinder mappings
noremap <silent><Leader>nf :FufFileWithCurrentBufferDir<CR>
noremap <silent><Leader>nd :FufDir<CR>
noremap <silent><Leader>nt :FufCoverageFile<CR>

" ZoomWin configuration
map <silent><Leader><Leader> :ZoomWin<CR>

" Syntastic configuration
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_enable_signs=1
" errors split closes when no errors left
let g:syntastic_auto_loc_list=2
