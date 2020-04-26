" Vim color file
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last Change:	2001 Jul 23

" This is the default color scheme.  It doesn't define the Normal
" highlighting, it uses whatever the colors used to be.

" Set 'background' back to the default.  The value can't always be estimated
" and is then guessed.
hi clear Normal
set bg&

" Remove all existing highlighting and set the defaults.
hi clear

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
  syntax reset
endif

let colors_name = "gruvbox"

hi Comment      ctermfg=8
hi Constant     ctermfg=6
hi String       ctermfg=6
hi Special      ctermfg=none
hi Identifier   ctermfg=none
hi Statement    ctermfg=none   cterm=bold
hi PreProc      ctermfg=none
hi type         ctermfg=none   cterm=bold
hi Underlined   ctermfg=none
hi Ignore       ctermfg=none
hi Keyword      ctermfg=none   cterm=bold
hi Label        ctermfg=none
hi Repeat       ctermfg=none
hi Error        ctermbg=1

" Golang
hi goDirective  ctermfg=none   cterm=bold
hi goDeclType   ctermfg=none

hi LineNR ctermfg=8

" vim: sw=2
