set nocompatible  " be iMproved
filetype off " vundle required!
behave mswin

syntax on " syntax highlighting
set nu " line number
set ts=4
set sw=4
set softtabstop=4
set expandtab " tab을 space로 저장
set scrolloff=15 " scroll offset. 15줄 범위에서부터 scroll
set ruler " 현재 row, col을 출력
set autoindent
set cindent
set hlsearch " 검색 결과 강조
set ignorecase " 대소문자 무시 검색
set smartcase " 대문자가 검색 문자열에 있을때는 noignorecase

" sound, visual bell 둘 다 비활성화.
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" windows gvim에서 utf8로 인코딩 설정시 메시지가 깨져서 영문 메시지를 사용
set enc=utf-8
set fencs=ucs-bom,utf-8,cp949
set fenc=utf-8
" mac에서 명령어 인식을 못한다. 원인은 모르겠다.
if has("win32")
	lang mes en
endif

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle' 
Bundle 'EasyMotion' 
Bundle 'scrooloose/nerdtree'
if has("win32")
	Bundle 'PProvost/vim-ps1'
else
	Bundle 'nelstrom/vim-markdown-preview'
endif
" vundle http://kldp.org/node/125263 
" EasyMotion http://bit.ly/sXJJpS 

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

" powershell에서 vim을 열었을때, syntax highlighting이 안돼서 추가.
" 참고로 vim-ps1/ftdetect/ps1.vim에 정의되어 있다. 
" 왜 적용이 안 되는지는 모르는 상태
au BufNewFile,BufRead *.ps1 set ft=ps1

