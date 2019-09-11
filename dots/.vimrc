"        _
" __   _(_)_ __ ___  _ __ ___
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__
"   \_/ |_|_| |_| |_|_|  \___|

let mapleader =" "

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

" enable man pages in vim
	runtime ftplugin/man.vim

" Window Navigation
	nnoremap <C-h> <C-w>h
	nnoremap <C-j> <C-w>j
	nnoremap <C-k> <C-w>k
	nnoremap <C-l> <C-w>l

	noremap <C-x>` :cn<CR>
	noremap <C-x><C-c> :qa<CR>

	noremap <leader>t :vertical terminal<CR>
