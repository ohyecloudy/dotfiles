#!/bin/bash

# 실행되는 쉘 절대 경로 구하기
# http://kldp.org/node/119861
cd ${0%/*} 2>/dev/null
shell_path=$PWD/${0##*/}

# TODO: 이거 한번에 하는 방법 없나?
script_path=${shell_path%/*}
root_path=${script_path%/*}

echo "submodule init/update"
cd $root_path
git submodule update --init

echo "create a vimrc symbolic link"
ln -s $root_path/vim/vimrc ~/.vimrc

echo "create a gitconfig symbolic link"
ln -s $root_path/git/gitconfig ~/.gitconfig

