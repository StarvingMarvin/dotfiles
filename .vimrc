syntax on
set nowrap
set hidden
set number

set autoindent
set smartindent
set pastetoggle=<F2>

set hlsearch
set incsearch

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

if has("autocmd")

  filetype on
 
  autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
  autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
 
endif

set background=dark
set mouse=a

set list listchars=tab:»·,trail:·,extends:»
highlight NonText guifg=#444455
highlight SpecialKey guifg=#444455
