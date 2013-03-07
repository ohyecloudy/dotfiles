#!/bin/bash

echo customise mac
# 파인더에서 전체 경로 보기
defaults write com.apple.finder _FXShowPosixPathInTitle -bool TRUE; killall Finder
# defaults write com.apple.finder _FXShowPosixPathInTitle -bool FALSE; killall Finder

# change the screen capture file format
defaults write com.apple.screencapture type png; killall SystemUIServer 

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

echo "create symbolic link"
ln -s $root_path/vim/vimrc ~/.vimrc

