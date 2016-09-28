
" General Settings {{{

set nocompatible
let mapleader = ","
let maplocalleader = ","

" display statusline on every window
set laststatus=2

" directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" use + register for copy/paste
set clipboard=unnamedplus

" minimum lines to keep above/below cursor
set scrolloff=3

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
set nowrap
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
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

" split to the right / below current window
set splitright
set splitbelow

" highlight line length
set colorcolumn=120

" }}}

" Adjust defaults {{{

" automatically read changed files
set autoread
" adjust indentation of wrapped lines
set breakindent
" remap leader
nnoremap <Leader>; ,
" don't move cursor on "*"
nnoremap * *<c-o>
map Oj *
imap Oj *
cmap Oj *
" search for visual selection
vnoremap * y/<c-r>"<cr><c-o>
" disable displaying manual when hitting K
nnoremap K <nop>
" disable C-G (avoids vim freezes when in tmux)
inoremap <C-G> <nop>
" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv
" When pasting, auto-adjust indent and jump to end
nnoremap p ]p`]
vnoremap p ]p`]
vnoremap y y`]
nnoremap P ]P
" Navigate to next diff after action
nmap do do]c
nmap dp dp]c
" Make Y behave like other capitals
map Y y$
" Improve up/down movement on wrapped lines
nnoremap j gj
nnoremap k gk
" Keep the search matches in the middle of the window
nnoremap n nzzzv
nnoremap N Nzzzv

" allows cursor change in tmux mode
if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
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
vnoremap <C-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <C-k> :m'<-2<cr>`>my`<mzgv`yo`z

" More fold/unfold mappings
nnoremap <Space> za
vnoremap <Space> za
nnoremap zJ zjzmza
nnoremap zK zkzmza[z

" window shortcuts
nmap <silent>= :res<CR>:vertical res<CR>
nmap <Leader>= <C-w>=
nmap <Leader>l <C-w>L
nmap <Leader>p <C-w>J

" mappings for navigating the location list
map <Leader>J :lfirst<CR>
map <Leader>j :lnext<CR>
map <Leader>K :llast<CR>
map <Leader>k :lprevious<CR>

" configure extra mappings for fugitive's Gdiff view
noremap <silent><Leader>d ]czz
noremap <silent><Leader>D [czz
" close Gdiff (or Gblame) view
noremap <silent><Leader>q :x<CR><C-w>j

" Sudo write
cmap w!! w !sudo tee % > /dev/null <CR>

" CTags
map <Leader>rt :!ctags --extra=+f -R *<CR><CR>

" HTML escape the content of Visual selection
vnoremap <Leader>x :!recode utf8..html<CR>

" Diff contents of registers 'a and 'b
nnoremap <silent><Leader>V :tabnew<CR>:put a<CR>:diffthis<CR>:vnew<CR>:put b<CR>:diffthis<CR>
nnoremap <silent><Leader>Q :tabclose!<CR>

" }}} 

" Filetype Specific Rules {{{
augroup configgroup
   autocmd!

   " fold vim files around {{{ ... }}}
   au FileType vim setlocal foldmethod=marker

   " Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
   au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby

   " Some filetypes need 4-space tabs
   au FileType {python,haskell,markdown} set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

   " some filetypes need real tabs
   au FileType {make,gitconfig} set noexpandtab

   " handlebars templates are HTML
   au BufRead,BufNewFile *.handlebars set filetype=html

  " fold Gruntfile.js for easier reading
   au BufRead,BufNewFile Gruntfile.js g/^\s\{4}\S*:\s\={\|registerTask/norm $zf%

   " use xmllint to format xml
   au FileType xml set equalprg=xmllint\ --format\ -

   " ES6!
   au BufNewFile,BufRead *.es6 set ft=javascript

   " handy mapping to :Eval when in Clojure
   au FileType clojure nnoremap <silent><Leader>e :Eval<CR>
augroup END
" }}}

" Load plugins / Apply customizations {{{

call plug#begin('~/.vim/plugged')

Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'flazz/vim-colorschemes'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

Plug 'kien/ctrlp.vim'
Plug 'd11wtq/ctrlp_bdelete.vim'
Plug 'scrooloose/nerdtree', { 'on':  ['NERDTreeFind', 'NERDTreeToggle'] }
Plug 'benmills/vimux', { 'on': [] }

Plug 'tpope/vim-commentary'
Plug 'scrooloose/syntastic'
Plug 'editorconfig/editorconfig-vim'

Plug 'rking/ag.vim'
Plug 'sjl/gundo.vim'
Plug 'edsono/vim-matchit'
Plug 'Lokaltog/vim-easymotion'
Plug 'diepm/vim-rest-console', { 'for': 'rest' }

Plug 'moll/vim-node'
Plug 'othree/yajs.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'digitaltoad/vim-jade', { 'for': 'pug' }

Plug 'derekwyatt/vim-scala', { 'for': ['scala', 'sbt.scala'] }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'mattn/emmet-vim', { 'for': ['html', 'css'] }
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'groenewege/vim-less', { 'for': 'less' }
Plug 'rodjek/vim-puppet', { 'for': 'puppet' }

Plug 'ervandew/supertab'

" Add plugins to &runtimepath
call plug#end()

" shortcut for PlugStatus
nnoremap <silent><Leader>S :PlugStatus<CR>

" default colorscheme
set background=dark
let g:solarized_contrast="high"
colorscheme solarized

" set airline plugin to use fancy symbols
let g:airline_powerline_fonts = 1

" set supertab completion scheme
set completeopt=longest,menuone,preview
let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabLongestHighlight = 1

" configure NERDTree toggler
let g:NERDTreeWinSize=40
let g:NERDTreeMinimalUI=1
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeAutoDeleteBuffer=1
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '-'
nnoremap <silent><Leader>nd :NERDTreeToggle<CR>
nnoremap <silent><Leader>nf :NERDTreeFind<CR>

" configure gundo
nnoremap <Leader>u :GundoToggle<CR>
let g:gundo_preview_bottom=1
let g:gundo_help=0
let g:gundo_close_on_revert=1

" configure ctrlp mappings
let g:ctrlp_mruf_max = 4096
let g:ctrlp_working_path_mode = 'a'
let g:ctrlp_use_caching = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_match_window = 'max:30'
set wildignore+=*/target/*,*/node_modules*/*,*/vimundo/*
noremap <silent><Leader>nt :CtrlP<CR>
noremap <silent><Leader>nb :CtrlPBuffer<CR>
noremap <silent><Leader>ne :CtrlPMRUFiles<CR>
call ctrlp_bdelete#init()

" Ag
noremap <silent><Leader>G :AgFromSearch<CR>

" REST Console
let g:vrc_set_default_mapping = 0
" enable persistent cookies
let g:vrc_cookie_jar = $HOME . '/.vim/backup/vrc_cookie_jar'
au BufNewFile,BufRead *.rest nmap <silent><c-i> :call VrcQuery()<CR>
" calls current API block in debug mode
au BufNewFile,BufRead *.rest nmap <silent><c-j> :let b:vrc_debug=1<CR><c-i>:let b:vrc_debug=0<CR>

" Remap vim-commentary
map ,c gc

" fix nasty vimux bug with ruby1.9
ruby << EOF
class Object
  def flush; end unless Object.new.respond_to?(:flush)
end
EOF

" configure vimux
let g:VimuxHeight = "35"
let g:VimuxOrientation = "h"
noremap <silent>! :VimuxPromptCommand<CR>
noremap <silent><Leader>rl :VimuxRunLastCommand<CR>
noremap <silent><Leader>rr :call VimuxRunCommand("", 0)<CR>
noremap <silent><Leader>ri :VimuxInspectRunner<CR>
noremap <silent><Leader>rx :VimuxCloseRunner<CR>
noremap <silent><Leader>rc :VimuxInterruptRunner<CR>
vnoremap <silent><Leader>rv "vy :call VimuxRunCommand(@v . "\n")<CR>

" Syntastic configuration
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_signs=1
" errors split closes when no errors left
let g:syntastic_auto_loc_list=2
let g:syntastic_error_symbol = '✗✗'
let g:syntastic_style_error_symbol = '✗✗'
let g:syntastic_warning_symbol = '≈≈'
let g:syntastic_style_warning_symbol = '≈≈'

let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': ['java'] }
" ES6 support
au BufNewFile,BufRead *.es6 let g:syntastic_javascript_checkers = ['eslint']
au BufNewFile,BufRead *.js let g:syntastic_javascript_checkers = ['jshint']

if filereadable(expand("~/.vimrc.extras"))
    source ~/.vimrc.extras
endif

" }}}

" OS X specific settings {{{
if has("mac")
  set clipboard=unnamed
endif
  
" }}}
