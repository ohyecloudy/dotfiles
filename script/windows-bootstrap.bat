@echo submodule update

cd /d %homedrive%%homepath%\.dotfiles
git submodule update --init 

@echo create symbolic link

mklink %homedrive%%homepath%\.vimrc %homedrive%%homepath%\.dotfiles\vim\vimrc

pause

