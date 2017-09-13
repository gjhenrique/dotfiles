set nocompatible              " be iMproved, required
filetype off                  " required

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

set clipboard=unnamed
set clipboard=unnamedplus

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
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar

noremap <Leader>y "+y
noremap <Leader>p "+p

if filereadable('.vimrc.local')
    source .vimrc.local
endif
