@echo submodule update

cd /d %homedrive%%homepath%\.vim
git submodule update --init 

@echo create symbolic link

mklink %homedrive%%homepath%\.vimrc %homedrive%%homepath%\.vim\vimrc

pause

