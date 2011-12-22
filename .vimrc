set nocompatible  " be iMproved
filetype off " vundle required!
behave mswin

syntax on " syntax highlighting
set nu " line number
set ts=4
set sw=4
set softtabstop=4
set autoindent

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'


if has('gui_running')
	set guioptions-=T " no toolbar
endif

filetype plugin indent on " vundle required!
