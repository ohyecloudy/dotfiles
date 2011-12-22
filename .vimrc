set nocompatible  " be iMproved
filetype off " vundle required!
behave mswin

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'


if has('gui_running')
	set guioptions-=T " no toolbar
endif

filetype plugin indent on " vundle required!
