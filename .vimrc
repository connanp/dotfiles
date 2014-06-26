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

" tjump for CtrlP
nnoremap <c-]> :CtrlPtjump<cr>
vnoremap <c-]> :CtrlPtjumpVisual<cr>

" automatically display all buffers when only one tab open
let g:airline#extensions#tabline#enabled = 1

" Stop CtrlP from recalculating on files on start
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_root_markers = ['.root', 'Makefile', '.git' ]
let g:ctrlp_regexp = 1

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
set nowrap
set linebreak " Wrap at word


" File Stuff ******************************************************************
"autocmd FileType html :set filetype=xhtml


" Misc settings ***************************************************************
" Optimize for fast terminal connections
set ttyfast

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
noremap <leader>ss :call StripWhitespace()<CR>
" Save a file as root (,W)
noremap <leader>W :w !sudo tee % > /dev/null<CR>

" change working directory to current file
nnoremap ,cd :cd %:p:h<CR>:pwd<CR>

" Automatic commands
if has("autocmd")
  " Enable file type detection
  filetype on
  " Treat .json files as .js
  autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
endif

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

if filereadable(expand('~/.vim/extra'))
  execute 'source ~/.vim/extra'
endif
