#!/bin/bash

echo customise mac
# 파인더에서 전체 경로 보기
defaults write com.apple.finder _FXShowPosixPathInTitle -bool TRUE; killall Finder

# change the screen capture file format
defaults write com.apple.screencapture type png; killall SystemUIServer 

# dashboard 사용 안 하기
defaults write com.apple.dashboard mcx-disabled -boolean YES; killall Dock

# 파인더에 숨은 파일 표시하기
defaults write com.apple.finder AppleShowAllFiles TRUE; killall Finder

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
ln -s $root_path/vim/xvimrc ~/.xvimrc

echo "make hlink"
gcc $root_path/mac/hlink.c -o $root_path/mac/hlink 

echo "create hard linked directory"
$root_path/mac/hlink $root_path/emacs.d ~/.emacs.d

ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
brew install leiningen
brew install --override-system-vim macvim

