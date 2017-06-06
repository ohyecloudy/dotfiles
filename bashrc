# -*- mode: sh -*-

if [[ "$MSYSTEM" == "MSYS" ]]; then
    export PATH=/c/Users/ohyecloudy/bin:/mingw64/bin:/usr/local/bin:/usr/bin:/bin:/opt/bin:$ORIGINAL_PATH
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export PATH="$HOME/bin.local:$HOME/bin:$PATH";
fi

source ~/.dotfiles/git/git-prompt.sh
source ~/.dotfiles/git/git-completion.bash

# https://github.com/git-for-windows/build-extra/blob/master/git-extra/git-prompt.sh

GREEN='\[\033[32m\]'
PUPPLE='\[\033[35m\]'
BROWNISH_YELLOW='\[\033[33m\]'
CYAN='\[\033[36m\]'
NO_COLOR='\[\033[0m\]'

if [[ "$EMACS" == "t" ]]; then
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
