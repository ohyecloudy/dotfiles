if [[ "$MSYSTEM" == "MSYS" ]]; then
    export PATH=/c/Users/ohyecloudy/bin:/mingw64/bin:/usr/local/bin:/usr/bin:/bin:/opt/bin:$ORIGINAL_PATH
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export PATH="$HOME/bin.local:$HOME/bin:$PATH";
fi
