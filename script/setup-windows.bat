@echo submodule update

cd /d %homedrive%%homepath%\vimrc
git submodule update --init 

@echo create symbolic link

mklink %homedrive%%homepath%\.vimrc %homedrive%%homepath%\vimrc\.vimrc

pause

