setlocal

set GIT_BASH_COMMAND="C:\git-sdk-64\git-cmd.exe" --no-cd --command=usr/bin/bash.exe -l -i

%GIT_BASH_COMMAND% -c ",session-close"

endlocal
