set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install bundles
"let path = '~/some/path/here'
"call vundle#rc(path)

" let Vundle manage Vundle, required
Plugin 'gmarik/vundle'

Plugin 'scroolose/syntastic'
Plugin 'bling/vim-airline'
Plugin 'scrooloose/nerdtree'
Plugin 'Valloric/YouCompleteMe'
Plugin 'xuhdev/SingleCompile.git'
Plugin 'kien/ctrlp.vim'
Plugin 'tacahiroy/ctrlp-funky'
Plugin 'chase/vim-ansible-yaml'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'vim-scripts/vim-auto-save'
Plugin 'scrooloose/nerdcommenter'
Plugin 'Raimondi/delimitMate'
Plugin 'jplaut/vim-arduino-ino'
Plugin 'vim-scripts/avr.vim'

" Vim snippets
Plugin 'SirVer/ultisnips'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'
" Most Recently Used
Plugin 'coot/atp_vim'
Plugin 'chriskempson/base16-vim'
Plugin 'smancill/conky-syntax.vim'
Plugin 'Valloric/MatchTagAlways'

Plugin 'jdevera/vim-opengrok-search'

Plugin 'tclem/vim-arduino'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install (update) bundles
" :BundleSearch(!) foo - search (or refresh cache first) for foo
" :BundleClean(!)      - confirm (or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle commands are not allowed.
" Put your stuff after this line

set laststatus=2
syntax enable
set number

set softtabstop=4
set shiftwidth=4
set tabstop=4
set expandtab
set noswapfile
set hidden

set splitright

let mapleader=","
map <M-1> :NERDTreeToggle<CR>
cnoremap sudow w !sudo tee % >/dev/null
imap <C-BS> <C-W>

imap <C-s> <C-o>:w<CR>
map <C-s> :w<CR>

" Copy and paste
vmap <C-c> "+yi
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <C-r><C-o>+

" Window Management
map <M-a>j <C-w>j 
map <M-a>k <C-w>k
map <M-a>l <C-w>l
map <M-a>h <C-w>h
map <M-a>m <C-w>_
map <M-s> <C-w>s
map <M-v> <C-w>v

" Replicate tabs with buffers
map <M-l> :bnext<CR>
map <M-h> :bprevious<CR>
" Close the current buffer and move to the previous one
" This replicates the idea of closing a tab
nmap <M-w> :bp <BAR> bd #<CR>
" Enable the list of buffers

map <C-h> :vert help

nmap <M-q> :q<CR>

nmap <F9> :SCCompile<cr>
nmap <F10> :SCCompileRun<cr>

nnoremap <M-f> :CtrlPFunky<Cr>
nnoremap Q <nop>

:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar

let g:airline#extensions#tabline#enabled = 1
let g:auto_save = 1  " do not change the 'updatetime' option

let g:ycm_confirm_extra_conf = 0
let g:ycm_warning_symbol = '>'
let g:ycm_autoclose_preview_window_after_completion = 1

" E501 => line too long
let g:syntastic_python_checker_args='--ignore=E501,E225'

" The directory of the current file
let g:ctrlp_working_path_mode = ''

let g:ctrlp_show_hidden = 1
let g:ctrlp_extensions = ['funky']

let NERDTreeQuitOnOpen = 1
set winaltkeys=no


if filereadable('.vimrc.local')
    source .vimrc.local
endif
