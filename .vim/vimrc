" vimrc - cjonsmith
set nocompatible
call pathogen#infect()
filetype on
filetype plugin on
filetype indent on

" -- Display
set title           " Update the title of the window
set relativenumber  " Display relative line numbers
set ruler           " Display cursor position
set wrap            " Word-wrap when too lines are too long

set scrolloff=3     " Display three lines around your cursor

" -- Search
set smartcase       " Ignore case if all lowercase

set incsearch       " Highlight search results while typing
set hlsearch        " Highlight other instances of searched phrase

" No beeping!
set belloff=all

" Useful backspaces
set backspace=indent,eol,start

" Hide backup file instead of abandoning
set hidden

" Use light version of Solarized if using GUI
if has ('gui_running')
	set background=light
	colorscheme solarized
endif

" Fonts
if has ('gui_macvim')
	set guifont=Monaco:h13
else
	set guifont=DejaVu\ Sans\ Mono\ 11
endif

" Autostart NERDTree
autocmd vimenter * NERDTree

