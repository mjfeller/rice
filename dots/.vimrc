"        _
" __   _(_)_ __ ___  _ __ ___
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__
"   \_/ |_|_| |_| |_|_|  \___|

let mapleader =" "

call plug#begin('~/.vim/plugged')
Plug 'Valloric/YouCompleteMe'
Plug '/usr/local/opt/fzf'
Plug 'rust-lang/rust.vim'
Plug 'junegunn/goyo.vim'
Plug 'fatih/vim-go'
call plug#end()

map <leader>g :Goyo<CR>

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

" fzf
	noremap <C-x><C-f> :FZF<CR>
	noremap <C-x>b :Buffers<CR>
	noremap <C-x><C-s> :w<CR>
	nmap <C-x><C-s> :w<CR>
	noremap <C-c>psr :Rg<CR>
	noremap <C-c>pf :GFiles<CR>
	nmap <C-s> :BLines<CR>
	autocmd! FileType fzf
	autocmd  FileType fzf set laststatus=0 noshowmode noruler
	  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

	noremap <C-x>` :cn<CR>
	noremap <C-x><C-c> :qa<CR>
	noremap <C-c><C-c> :GoBuild

	command! Core execute ":cd $CORE"


	nnoremap gi :GoImports<CR>

	noremap <leader>t :vertical terminal<CR>