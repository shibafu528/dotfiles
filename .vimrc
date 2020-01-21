syntax on

set nocompatible
set nobackup
set noundofile

set number
set nowrap
set cursorline

set expandtab
set tabstop=2
set shiftwidth=2
set autoindent
set smartindent

set ignorecase
set smartcase
set incsearch
set hlsearch

set laststatus=2
set tw=0

set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis

if has("autocmd")
  filetype on
  autocmd FileType * setlocal ts=4 sts=4 sw=4 expandtab
  autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
  autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
  autocmd FileType vim setlocal ts=2 sts=2 sw=2 expandtab
  autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab
  autocmd FileType css setlocal ts=2 sts=2 sw=2 expandtab
  autocmd FileType nginx setlocal ts=4 sts=4 sw=4 noexpandtab
endif

"set mouse=a
set mouse=
