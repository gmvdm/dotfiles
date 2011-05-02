if has("gui_running")
    colorscheme desert
	" hide mouse while typing
	set mousehide
    set guifont=Panic_Sans:h14
    set background=dark
else 
    " Use this to enable bold, but not color
    "set term=vt102
    " Use this to enable color
    " set term=xterm-color
    " colorscheme delek
    " colorscheme pablo
    " colorscheme ron
    colorscheme desert256
    set background=dark
endif

set ruler
set textwidth=78
set backupext=.bak
set ignorecase
set smartcase
set ts=4
set shiftwidth=4
set laststatus=2

" TODO Need to disable for makefiles ...
set expandtab

set hlsearch
set showmatch

" Mercury
" filetype on
" filetype plugin on
" syntax enable

syn on
filetype plugin indent on
