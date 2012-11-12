#!/bin/bash

echo "submodule init/update"
cd ~/.vim
git submodule update --init

echo "create symbolic link"
ln -s ~/.vim/vimrc ~/.vimrc

