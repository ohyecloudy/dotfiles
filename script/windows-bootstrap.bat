@powershell -NoProfile -ExecutionPolicy unrestricted -File %homedrive%%homepath%\.dotfiles\script\windows-bootstrap.ps1

@echo submodule update

cd /d %homedrive%%homepath%\.dotfiles
git submodule update --init 

@echo set home

setx HOME %homedrive%%homepath%

@echo create vimrc symbolic link

mklink %homedrive%%homepath%\.vimrc %homedrive%%homepath%\.dotfiles\vim\vimrc

@echo create emacs directory junction

mklink /J %homedrive%%homepath%\.emacs.d %homedrive%%homepath%\.dotfiles\emacs.d

pause

