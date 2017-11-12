
" General settings {{{

set nocompatible
let mapleader = ","
let maplocalleader = ","

" display statusline on every window
set laststatus=2

" directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" use + register for copy/paste
set clipboard=unnamed

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
" set Vim-specific sequences for RGB colors
" see https://github.com/vim/vim/issues/993
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" don't redraw while executing macros
set lazyredraw

" enable hidden unsaved buffers
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
set listchars=tab:»\ ,eol:¬,extends:❯,precedes:❮
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

" fold vim files
au FileType vim setlocal foldmethod=marker

" }}}

" Adjust defaults {{{

" don't bother me when a swap file exists
set shortmess+=A
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
" Increase the amount of remembered old files
set viminfo='1024

" allows cursor change in tmux mode
if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" }}}

" Custom mappings {{{

" Emacs style Home/End in insert/command mode
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" <Space> toggles folding
nnoremap <Space> za
vnoremap <Space> za

" enable quick navigation between folds
nnoremap zJ zjzmza
nnoremap zK zkzmza[z

" window shortcuts
nmap <silent>= :res<CR>:vertical res<CR>
nmap <Leader>= <C-w>=
nmap <Leader>l <C-w>L
nmap <Leader>p <C-w>J

" handier navigation between diffs
noremap <silent><Leader>d ]czz
noremap <silent><Leader>D [czz

" close buffer and go up
noremap <silent><Leader>q :x<CR><C-w>j

" Sudo write
cmap w!! w !sudo tee % > /dev/null <CR>

" HTML escape the content of Visual selection
vnoremap <Leader>x :!recode utf8..html<CR>

" Diff contents of registers 'a and 'b
nnoremap <silent><Leader>V :tabnew<CR>:put a<CR>:diffthis<CR>:vnew<CR>:put b<CR>:diffthis<CR>
nnoremap <silent><Leader>Q :windo bd!<CR>tabclose<CR>

" Toggle 'ignore whitespace' in vimdiff
set diffopt+=iwhite
command ToggleIWhite if &diffopt =~ 'iwhite' | set diffopt-=iwhite | else | set diffopt+=iwhite | endif
nnoremap <silent><Leader>w :ToggleIWhite<CR>

" }}} 

" Load plugins / Apply customizations {{{

call plug#begin('~/.vim/plugged')

Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'flazz/vim-colorschemes'

Plug 'tpope/vim-fugitive'
Plug 'shumphrey/fugitive-gitlab.vim'

Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

Plug 'mileszs/ack.vim'
Plug 'ervandew/supertab'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on':  ['NERDTreeFind', 'NERDTreeToggle'] }

Plug 'tmhedberg/matchit'
Plug 'Lokaltog/vim-easymotion'

Plug 'diepm/vim-rest-console', { 'for': 'rest' }

Plug 'w0rp/ale'

Plug 'moll/vim-node'
Plug 'othree/yajs.vim', { 'tag': '1.6' }
Plug 'othree/javascript-libraries-syntax.vim'

Plug 'tpope/vim-commentary'
Plug 'sheerun/vim-polyglot'

Plug 'tmux-plugins/vim-tmux-focus-events'

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
let g:SuperTabLongestEnhanced = 1

let g:fugitive_gitlab_domains = ['https://gitlab.adam']

" configure NERDTree toggler
let g:NERDTreeWinSize=40
let g:NERDTreeMinimalUI=1
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeAutoDeleteBuffer=1
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '-'
nnoremap <silent><Leader>nd :NERDTreeToggle<CR>
nnoremap <silent><Leader>nf :NERDTreeFind<CR>

" configure FZF
noremap <silent><Leader>nt :call fzf#run(fzf#wrap({'source': 'ag -g ""'}))<CR>
noremap <silent><Leader>nb :Buffers<CR>
noremap <silent><Leader>ne :History<CR>
noremap <silent><Leader>nr :History:<CR>
noremap <silent><Leader>nc :BCommits!<CR>
let g:fzf_layout = { 'down': '~30%' }
let g:fzf_action = {
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit',
  \ 'ctrl-d': 'vertical diffsplit' }

" Ag
let g:ackprg = 'ag --vimgrep'
" searches for the word under the cursor
nnoremap <silent><Leader>G :Ack! "\b<cword>\b"<CR>

" REST Console
let g:vrc_set_default_mapping = 0
let g:vrc_allow_get_request_body = 1
let g:vrc_elasticsearch_support = 1
let b:vrc_response_default_content_type = 'application/json'
let g:vrc_curl_opts = {
  \ '-b': $HOME . '/.vim/backup/vrc_cookie_jar',
  \ '-c': $HOME . '/.vim/backup/vrc_cookie_jar',
  \ '-s': '',
  \ '-L': '',
  \ '-k': '',
\}
au BufNewFile,BufRead *.rest nmap <silent><c-i> :call VrcQuery()<CR>

" Remap vim-commentary
map ,c gc

" Enable quotes concealing for JSON files
let g:vim_json_syntax_conceal = 1

" ALE configuration
let g:ale_lint_on_text_changed = 'never'
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_sign_error = '✗✗'
let g:ale_sign_highlights = 1 
let g:ale_sign_warning = '▲'
let g:airline#extensions#ale#enabled = 1
let g:ale_javascript_eslint_options = '--quiet'
let g:ale_pattern_options = {
\   '\.js$': {
\     'ale_linters': {'javascript': ['eslint']},
\     'ale_fixers': {'javascript': ['eslint']}
\   },
\   '\.handlebars$': {
\     'ale_enabled': 0
\   },
\}
nmap <silent><Leader>f <Plug>(ale_fix)
nmap <silent><Leader>J <Plug>(ale_first)
nmap <silent><Leader>j <Plug>(ale_next_wrap)
nmap <silent><Leader>k <Plug>(ale_previous_wrap)
nmap <silent><Leader>K <Plug>(ale_last)

" }}}
