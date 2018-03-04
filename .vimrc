execute pathogen#infect()
syntax on
set ruler
set showcmd
set softtabstop=4
set shiftwidth=4
set expandtab
set number
set relativenumber
set autoindent

" Set tab styles for different file types
" TODO: Do multiple commands for autocmd
autocmd BufNewFile,BufRead *.js,*.jsx,*.ts,*.tsx set tabstop=2
autocmd BufNewFile,BufRead *.js,*.jsx,*.ts,*.tsx set shiftwidth=2
autocmd BufNewFile,BufRead *.js,*.jsx,*.ts,*.tsx set expandtab
autocmd BufNewFile,BufRead *.py set tabstop=4
autocmd BufNewFile,BufRead *.py set shiftwidth=4
autocmd BufNewFile,BufRead *.py set expandtab

" Set 'keywordprg' to be relevant documentation for different filetypes
autocmd FileType git setlocal keywordprg=git\ show
autocmd FileType vim setlocal keywordprg=:help
autocmd FileType python setlocal keywordprg=pydoc

" Add all downward directories to our path
set path+=**

" Add window for fuzzy finding tab completion
set wildmenu

" Netrw file browser edits
let g:netrw_banner=0    " remove netrw banner
let g:netrw_keepdir=0   " current viewing dir is current working dir

