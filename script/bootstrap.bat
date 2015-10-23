@echo submodule update

cd /d %homedrive%%homepath%\.dotfiles
git submodule update --init 

@echo set home

setx HOME %homedrive%%homepath%

@echo git

mklink %homedrive%%homepath%\.gitignore_global %homedrive%%homepath%\.dotfiles\git\gitignore_global
mklink %homedrive%%homepath%\.gitconfig %homedrive%%homepath%\.dotfiles\git\gitconfig

@echo create vimrc symbolic link

mklink %homedrive%%homepath%\.vimrc %homedrive%%homepath%\.dotfiles\vim\vimrc

@echo create emacs directory junction

mklink /J %homedrive%%homepath%\.emacs.d %homedrive%%homepath%\.dotfiles\emacs.d

pause

