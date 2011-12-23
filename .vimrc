set nocompatible  " be iMproved
filetype off " vundle required!
behave mswin

syntax on " syntax highlighting
set nu " line number
set ts=4
set sw=4
set softtabstop=4
set autoindent

" windows gvim에서 utf8로 인코딩 설정시 메시지가 깨져서 영문 메시지를 사용
set enc=utf-8
set fencs=ucs-bom,utf-8,cp949
set fenc=utf-8
lang mes en

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'
Bundle 'fugitive.vim'

if has('gui_running')
	set guioptions-=T " no toolbar
	" windows gvim에서 encoding을 utf-8로 설정하면 메뉴가 깨져서 langmenu를
	" 설정해 준다. 좀 무식해 보여...
	if has("win32")
		source $VIMRUNTIME/delmenu.vim
		set langmenu=ko_kr.utf-8
		source $VIMRUNTIME/menu.vim
	endif
endif

filetype plugin indent on " vundle required!
