"        _
" __   _(_)_ __ ___  _ __ ___
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__
"   \_/ |_|_| |_| |_|_|  \___|

let mapleader =" "

call plug#begin('~/.vim/plugged')
Plug 'junegunn/goyo.vim'
Plug 'fatih/vim-go'
call plug#end()

" Some basics:
	set nocompatible
	set encoding=utf-8

" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow
	set splitright

" Compile document
	map <leader>c :!compiler <c-r>%<CR>

" Open corresponding .pdf
	map <leader>p :!open <c-r>%<CR><CR>
