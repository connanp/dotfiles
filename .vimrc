set nocompatible

filetype off  " required for vundle
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Plugins
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'ivalkeen/vim-ctrlp-tjump'
Plugin 'scrooloose/syntastic'
Plugin 'scrooloose/nerdcommenter'
Plugin 'bling/vim-airline'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'sjl/gundo.vim'
Plugin 'majutsushi/tagbar'
Plugin 'sheerun/vim-polyglot'
Plugin 'rking/ag.vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-endwise'
Plugin 'Shougo/unite.vim'
"Plugin 'Shougo/neocomplete.vim'

call vundle#end()
filetype plugin indent on
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList          - list configured plugins
" :PluginInstall(!)    - install (update) plugins
" :PluginSearch(!) foo - search (or refresh cache first) for foo
" :PluginClean(!)      - confirm (or auto-approve) removal of unused plugins
"
" see :h vundle for more details or wiki for FAQ

" airline theme
let g:airline_theme='powerlineish'

" automatically display all buffers when only one tab open
let g:airline#extensions#tabline#enabled = 1

" Stop CtrlP from recalculating on files on start
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_root_markers = ['.root', 'Makefile', '.git' ]
let g:ctrlp_regexp = 1
let g:ctrlp_match_window = 'order:ttb,max:20'

let g:NERDSpaceDelims=1
let g:gitgutter_enabled = 0

let mapleader = ","
set wildignore=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.png,*.jpg,*.o,.DS_Store

" Tabs ************************************************************************
"set sta " a <Tab> in an indent inserts 'shiftwidth' spaces

function! Tabstyle_tabs()
" Using 4 column tabs
  set softtabstop=4
  set shiftwidth=4
  set tabstop=4
  set noexpandtab
  autocmd User Rails set softtabstop=4
  autocmd User Rails set shiftwidth=4
  autocmd User Rails set tabstop=4
  autocmd User Rails set noexpandtab
endfunction

function! Tabstyle_spaces()
" Use n spaces
  set softtabstop=2
  set shiftwidth=2
  set tabstop=2
  set expandtab
endfunction

call Tabstyle_spaces()

set shell=zsh
set encoding=utf-8 nobomb
set fillchars+=stl:\ ,stlnc:\
set guifont=Inconsolata\ for\ Powerline:h11
" disable this if you have issues
let g:airline_powerline_fonts = 1

" Indenting *******************************************************************
" set ai " Automatically set the indent of a new line (local to buffer)
" set si " smartindent (local to buffer)
set preserveindent
set copyindent
set pastetoggle=<F2>


" Cursor highlights ***********************************************************
set showmatch
set cursorline
"set cursorcolumn


" Searching *******************************************************************
set hlsearch " highlight search
set incsearch " Incremental search, search as you type
set ignorecase " Ignore case when searching
set smartcase " Ignore case when searching lowercase
" Add the g flag to search/replace by default
set gdefault

" ,a to continue search throughout all
nmap <Leader>a :silent exec "while !search( @/, \"W\") \| bnext \| 0 \| endwhile"<CR>

if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor\ --smart-case\ --follow

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" Colors **********************************************************************
"set t_Co=256 " 256 colors
set background=dark
syntax on " syntax highlighting
colorscheme ir_black


" Status Line *****************************************************************
set laststatus=2
set noshowmode
" Show command as it's being typed
set showcmd
set ruler " Show ruler
"set ch=2 " Make command line two lines high
"match LongLineWarning '\%120v.*' " Error format when a line is longer than 120


" Line Wrapping ***************************************************************
"set nowrap
set linebreak " Wrap at word


" File Stuff ******************************************************************

" Treat .json files as .js
autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
autocmd FileType html :set filetype=xhtml
" md is markdown
autocmd BufRead,BufNewFile *.md set filetype=markdown
autocmd BufRead,BufNewFile *.md set spell

" Key Mappings ***************************************************************
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>d :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFind<CR>
nnoremap <leader>t :CtrlP<CR>
nnoremap <leader>T :CtrlPClearCache<CR>:CtrlP<CR>
nnoremap <leader>] :TagbarToggle<CR>
nnoremap <leader>g :GitGutterToggle<CR>
noremap <silent> <leader>V :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" tjump for CtrlP
nnoremap <c-]> :CtrlPtjump<cr>
vnoremap <c-]> :CtrlPtjumpVisual<cr>

" Save a file as root (,W)
noremap <leader>W :w !sudo tee % > /dev/null<CR>

if has("gui_gtk2") || has("win32") || has("win64")
  " Press Ctrl-Tab to switch between open tabs (like browser tabs) to 
  " the right side. Ctrl-Shift-Tab goes the other way.
  noremap <C-Tab> :tabnext<CR>
  noremap <C-S-Tab> :tabprev<CR>

  " Switch to specific tab numbers with Command-number
  noremap <M-1> :tabn 1<CR>
  noremap <M-2> :tabn 2<CR>
  noremap <M-3> :tabn 3<CR>
  noremap <M-4> :tabn 4<CR>
  noremap <M-5> :tabn 5<CR>
  noremap <M-6> :tabn 6<CR>
  noremap <M-7> :tabn 7<CR>
  noremap <M-8> :tabn 8<CR>
  noremap <M-9> :tabn 9<CR>
  " Command-0 goes to the last tab
  noremap <M-0> :tablast<CR>
endif

if has("gui_macvim")
  " Press Ctrl-Tab to switch between open tabs (like browser tabs) to 
  " the right side. Ctrl-Shift-Tab goes the other way.
  noremap <C-Tab> :tabnext<CR>
  noremap <C-S-Tab> :tabprev<CR>

  " Switch to specific tab numbers with Command-number
  noremap <D-1> :tabn 1<CR>
  noremap <D-2> :tabn 2<CR>
  noremap <D-3> :tabn 3<CR>
  noremap <D-4> :tabn 4<CR>
  noremap <D-5> :tabn 5<CR>
  noremap <D-6> :tabn 6<CR>
  noremap <D-7> :tabn 7<CR>
  noremap <D-8> :tabn 8<CR>
  noremap <D-9> :tabn 9<CR>
  " Command-0 goes to the last tab
  noremap <D-0> :tablast<CR>
endif

nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gb :Gblame<CR>
nnoremap <silent> <leader>gl :Glog<CR>
nnoremap <silent> <leader>gp :Git push<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>
nnoremap <silent> <leader>gr :Gremove<CR>
autocmd BufReadPost fugitive://* set bufhidden=delete

" Misc settings ***************************************************************
" Optimize for fast terminal connections
set ttyfast

" use pipes instead of buffers
set noshelltemp

" Omni completion (IntelliSense)
"set omnifunc=syntaxcomplete#Complete

" Enable basic mouse behavior such as resizing buffers.
set mouse=a
if exists('$TMUX')  " Support resizing in tmux
  set ttymouse=xterm2
endif

" Allow cursor keys in insert mode
set esckeys

" Allow backspace in insert mode
set backspace=indent,eol,start

" Centralize backups, swapfiles and undo history
set backupdir=~/.vim/backups
set directory=~/.vim/swaps
if exists("&undodir")
  set undodir=~/.vim/undo
endif

" Respect modeline in files
set modeline
set modelines=4

" Show “invisible” characters
set lcs=tab:▸\ ,trail:·,eol:¬,nbsp:_
set list

" Don’t reset cursor to start of line when moving around.
set nostartofline

" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamed

set number " Show line numbers
" Use relative line numbers
if exists("&relativenumber")
  set relativenumber
  au BufReadPost * set relativenumber
endif
" Start scrolling three lines before the horizontal window border
set scrolloff=3

set matchpairs+=<:>
set vb t_vb= " Turn off bell, this could be more annoying, but I'm not sure how
set nofoldenable " Turn off folding

" Strip trailing whitespace (,ss)
function! StripWhitespace()
  let save_cursor = getpos(".")
  let old_query = getreg('/')
  :%s/\s\+$//e
  call setpos('.', save_cursor)
  call setreg('/', old_query)
endfunction
noremap <leader><space> :call StripWhitespace()<CR>

" change working directory to current file
nnoremap ,cd :cd %:p:h<CR>:pwd<CR>

" faster macros processing
set lazyredraw

" persistend undo history
if has('persistent_undo')
  set undofile                " Save undo's after file closes
  set undodir=~/.vim/undo " where to save undo histories
  set undolevels=100         " How many undos
  set undoreload=1000        " number of lines to save for undo
endif

" not word dividers
set iskeyword+=_,$,@,%,#

" Show matching brackets
set sm

" Don't copy the contents of an overwritten selection.
vnoremap p "_dP

" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =

" Local configuration
if filereadable(expand('~/.vim/extra'))
  execute 'source ~/.vim/extra'
endif
