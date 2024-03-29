# -*- mode: sh -*-

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

if [[ "$MSYSTEM" == "MSYS" ]] || [[ "$MSYSTEM" == "MINGW"* ]]; then
    export PATH=/c/Users/ohyecloudy/bin:/mingw64/bin:/usr/local/bin:/usr/bin:/bin:/opt/bin:$ORIGINAL_PATH
    # ln 명령어가 full copy가 아닌 symbol link 생성으로 동작하게 한다.
    export MSYS=winsymlinks:nativestrict
    export ELIXIR_EDITOR="ec.bat +__LINE__ __FILE__"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # /opt/homebrew/bin/brew shellenv
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
    export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
    export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";

    export PATH="$HOME/.local/bin:$HOME/bin.local:$HOME/bin:$HOME/developer/flutter/bin:$HOME/.emacs.d/bin:$PATH";
    export ELIXIR_EDITOR="ec +__LINE__ __FILE__"
fi

if [[ "$OS" == "Windows_NT" ]]; then
    # UTF-8
    chcp.com 65001 >/dev/null
fi

export VISUAL=ec-wait
export EDITOR=ec-wait

# https://github.com/git-for-windows/build-extra/blob/master/git-extra/git-prompt.sh

GREEN='\[\033[32m\]'
PUPPLE='\[\033[35m\]'
BROWNISH_YELLOW='\[\033[33m\]'
CYAN='\[\033[36m\]'
NO_COLOR='\[\033[0m\]'

if [ "$TERM" == "dumb" ] || [ "$TERM" == "emacs" ]; then
    PS1=''
else
    TITLEPREFIX=$MSYSTEM
    PS1='\[\033]0;$TITLEPREFIX:${PWD//[^[:ascii:]]/?}\007\]' # set window title
    PS1="$PS1"'\n'             # new line
fi

PS1="$PS1"${GREEN}
PS1="$PS1"'\u@\h '             # user@host<space>
PS1="$PS1"${PUPPLE}
PS1="$PS1"'$MSYSTEM '          # show MSYSTEM
PS1="$PS1"${BROWNISH_YELLOW}
PS1="$PS1"'\w'                 # current working directory
PS1="$PS1"${CYAN}
PS1="$PS1"'`__git_ps1`'        # bash function
PS1="$PS1"${NO_COLOR}
PS1="$PS1"'\n'                 # new line
PS1="$PS1"'$ '                 # prompt: always $
MSYS2_PS1="$PS1"               # for detection by MSYS2 SDK's bash.basrc

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
command -v pyenv >/dev/null && eval "$(pyenv init -)"

# git completion
source ~/.dotfiles/git/git-prompt.sh
source ~/.dotfiles/git/git-completion.bash

# asdf
test -f /opt/homebrew/opt/asdf/libexec/asdf.sh && . /opt/homebrew/opt/asdf/libexec/asdf.sh
test -f /opt/homebrew/opt/asdf/etc/bash_completion.d/asdf.bash && . /opt/homebrew/opt/asdf/etc/bash_completion.d/asdf.bash
