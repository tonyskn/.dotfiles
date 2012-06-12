
" General Settings {{{

set nocompatible
let mapleader = ","
let maplocalleader = ","

" display statusline on every window
set laststatus=2

" directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" enable persistant undo
set udf
set undodir=~/.vim/vimundo

" disable error bell
set noeb vb t_vb=

" show command keystrokes in down/right corner
set showcmd

" default file encoding
set encoding=utf-8
set fileencoding=utf-8

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
set cursorline

" indentation and tabs
set expandtab
set tabstop=3
set shiftwidth=3
set autoindent
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
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

" }}}

" Adjust defaults {{{

" don't move cursor on "*"
nnoremap * *<c-o>
" disable displaying manual when hitting K
nnoremap K <nop>
" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv
" When pasting, auto-adjust indent
nnoremap p ]p
nnoremap P ]P
" Navigate to next diff after action
nnoremap do do[c]c
nnoremap dp dp[c]c
" Make Y behave like other capitals
map Y y$
" Improve up/down movement on wrapped lines
nnoremap j gj
nnoremap k gk
" Keep the search matches in the middle of the window
nnoremap n nzzzv
nnoremap N Nzzzv
" }}}

" Mappings {{{

" easy newline
nnoremap <CR> O<ESC>

" Emacs style Home/End
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

" Better comand-line editing
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" Line bubbling
noremap <C-j> mz:m+<cr>`z
noremap <C-k> mz:m-2<cr>`z
vnoremap <C-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <C-k> :m'<-2<cr>`>my`<mzgv`yo`z

" More fold/unfold mappings
nnoremap <Space> za
vnoremap <Space> za
nnoremap zJ zjzmza
nnoremap zK zkzmza[z

" window shortcuts
map <Leader>= <C-w>=
map <Leader>l <C-w>L
map <Leader>p <C-w>J

" mappings for navigating the location list
map <Leader>J :lfirst<CR>
map <Leader>j :lnext<CR>
map <Leader>K :llast<CR>
map <Leader>k :lprevious<CR>

" configure extra mappings for fugitive's Gdiff view
noremap <silent><Leader>d ]c
noremap <silent><Leader>D [c
noremap <silent><Leader>q <C-w>h:w<CR>:q<CR><C-w>k

" Sudo write
cmap w!! w !sudo tee % > /dev/null <CR>

" CTags
map <Leader>rt :!ctags --extra=+f -R *<CR><CR>

" }}} 

" Filetype Specific Rules {{{

" fold vim files around {{{ ... }}}
au FileType vim setlocal foldmethod=marker

" _zsh* files are Zsh scripts
au BufNewFile,BufRead *zsh* set ft=zsh

" JSON files are Javascript
au BufNewFile,BufRead *.json set ft=javascript

" Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby

" make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

" ghc uses 4-space tabs
au FileType haskell set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

" use 4-space tabs in markdown
au FileType markdown set softtabstop=4 tabstop=4 shiftwidth=4

" use xmllint to format xml
au FileType xml set equalprg=xmllint\ --format\ -
" use python json.tool to format JSON
au BufNewFile,BufRead *.json set equalprg=python\ -m\ json.tool

" some filetypes need real tabs
au FileType {make,gitconfig} set noexpandtab

" }}}

" Load plugins / Apply customizations {{{

" load pathogen
source ~/.vim/bundle/pathogen/autoload/pathogen.vim

" pathogen plugin, requires filetype plugin indent
filetype plugin indent on
call pathogen#infect()
" required to get help on stuff installed through pathogen
call pathogen#helptags()

" default colorscheme
set background=dark
colorscheme solarized
hi Normal ctermbg=none

" set powerline plugin to use fancy symbols
let g:Powerline_symbols = 'fancy'

" set supertab completion scheme
set completeopt=longest,menuone,preview
let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabLongestHighlight = 1

" easier Clam mappings
nnoremap ! :Clam<space>
vnoremap ! :ClamVisual<space>

" set slime to work in tmux mode
let g:slime_target = "tmux"

" configure FuzzyFinder mappings
let g:fuf_modesDisable=[]
noremap <silent><Leader>nn :FufRenewCache<CR>
noremap <silent><Leader>nf :FufFileWithCurrentBufferDir<CR>
noremap <silent><Leader>nd :FufDir<CR>
noremap <silent><Leader>nt :FufCoverageFile<CR>
noremap <silent><Leader>nb :FufBuffer<CR>
noremap <silent><Leader>ne :FufMruFile<CR>
noremap <silent><Leader>nc :FufMruCmd<CR>
noremap <silent><Leader>ng :FufLine<CR>
" open the latest search in :Fufline
noremap <silent><leader>/ :execute ':FufLine ' . substitute(substitute(substitute(@/, "\\\\<", "", ""), "\\\\>", "", ""), "\\\\v", "", "")<CR>

" fix nasty vimux bug with ruby1.9
ruby << EOF
class Object
  def flush; end unless Object.new.respond_to?(:flush)
end
EOF

" configure vimux
let VimuxHeight = "25"
noremap <silent><Leader>rp :PromptVimTmuxCommand<CR>
noremap <silent><Leader>rl :InterruptVimTmuxRunner<CR>:RunLastVimTmuxCommand<CR>
noremap <silent><Leader>ri :InspectVimTmuxRunner<CR>
noremap <silent><Leader>rx :CloseVimTmuxRunner<CR>
noremap <silent><Leader>rc :InterruptVimTmuxRunner<CR>
vnoremap <silent><Leader>rv "vy :call RunVimTmuxCommand(@v . "\n", 0)<CR>
au! VimLeavePre * :CloseVimTmuxRunner
"
" ZoomWin configuration
map <silent><Leader><Leader> :ZoomWin<CR>

" Syntastic configuration
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_enable_signs=1
" errors split closes when no errors left
let g:syntastic_auto_loc_list=2

" }}}
