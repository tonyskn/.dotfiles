
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

" }}}

" Adjust defaults {{{

" automatically read changed files
set autoread
" remap,
nnoremap <Leader>; ,
" don't move cursor on "*"
nnoremap * *<c-o>
map Oj *
imap Oj *
cmap Oj *
" search for visual selection
vnoremap * y/<C-R>"<CR>
" disable displaying manual when hitting K
nnoremap K <nop>
" disable C-G (avoids vim freezes when in tmux)
inoremap <C-G> <nop>
" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv
" When pasting, auto-adjust indent
nnoremap p ]p
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
" use python json.tool to format JSON
au BufNewFile,BufRead *.json set equalprg=python\ -m\ json.tool

" handy mapping to :Eval when in Clojure
au FileType clojure nnoremap <silent><Leader>e :Eval<CR>

" Auto format JSON curl output in REST console buffers
au BufEnter __REST_response__ silent exe "norm gg:set mad}:%!python -mjson.tool 2>&/dev/null || trueP:set noma"


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
let g:solarized_termtrans=1
" let g:solarized_termcolors=256
let g:solarized_contrast="high"
" let g:solarized_visibility="low"
colorscheme solarized
" hi Normal ctermbg=none

" set airline plugin to use fancy symbols
let g:airline_powerline_fonts = 1

" set supertab completion scheme
set completeopt=longest,menuone,preview
let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabLongestHighlight = 1

" easier Clam mappings
nnoremap <leader>! :Clam<space>
vnoremap <leader>! :ClamVisual<space>

" configure NERDTree toggler
let g:NERDTreeWinSize=40
let g:NERDTreeMinimalUI=1
let g:NERDTreeAutoDeleteBuffer=1
nnoremap <silent><Leader>nd :NERDTreeToggle<CR>
nnoremap <silent><Leader>nf :NERDTreeFind<CR>

" override sparkup mapping
let g:sparkupExecuteMapping = "<c-f>"

" configure FuzzyFinder mappings
let g:ctrlp_mruf_max = 4096
let g:ctrlp_working_path_mode = 'a'
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_match_window = 'max:30'
set wildignore+=*/target/*,*/node_modules/*,*/vimundo/*
noremap <silent><Leader>nt :CtrlP<CR>
noremap <silent><Leader>nn :CtrlPClearCache<CR>
noremap <silent><Leader>nb :CtrlPBuffer<CR>
noremap <silent><Leader>ne :CtrlPMRUFiles<CR>

" Ag
noremap <silent><Leader>G :AgFromSearch<CR>

" REST Console
let g:vrc_cookie_jar = $HOME . '/.vim/backup/vrc_cookie_jar'

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
au! VimLeavePre * :VimuxCloseRunner

" Syntastic configuration
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_signs=1
" errors split closes when no errors left
let g:syntastic_auto_loc_list=2
" let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': ['java'] }

if filereadable(expand("~/.vimrc.extras"))
    source ~/.vimrc.extras
endif

" }}}

" OS X specific settings {{{
if has("mac")
  set clipboard=unnamed
endif
  
" }}}
