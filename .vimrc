set nocompatible  " be iMproved
filetype off " vundle required!
behave mswin

syntax on " syntax highlighting
set nu " line number
set ts=4
set sw=4
set softtabstop=4
set autoindent
set cindent
set hlsearch " 검색 결과 강조
set noerrorbells
set novisualbell
set vb " mac에서 거슬리는 효과음을 비활성화

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
Bundle 'PProvost/vim-ps1'

" vundle http://kldp.org/node/125263 
" EasyMotion http://bit.ly/sXJJpS 
" nerdtree https://github.com/scrooloose/nerdtree
" ps1.vim https://github.com/PProvost/vim-ps1

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

map ㅂ q
map ㅈ w
map ㄷ e
map ㄱ r
map ㅅ t
map ㅛ y
map ㅕ u
map ㅑ i
map ㅐ o
map ㅔ p
map ㅁ a
map ㄴ s
map ㅇ d
map ㄹ f
map ㅎ g
map ㅗ h
map ㅓ j
map ㅏ k
map ㅣ l
map ㅋ z
map ㅌ x
map ㅊ c
map ㅍ v
map ㅠ b
map ㅜ n
map ㅡ m

