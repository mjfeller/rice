" Some basics:
	let mapleader =" "
	set nocompatible
	set encoding=utf-8

" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow
	set splitright

" enable man pages in vim
	runtime ftplugin/man.vim

" Window Navigation
	noremap <leader>t :vertical terminal<CR>
