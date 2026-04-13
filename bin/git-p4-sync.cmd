@echo off
setlocal

set "BASH_PATH=C:\git-sdk-64\usr\bin\bash.exe"
set "SCRIPT_WIN=%USERPROFILE%\bin\git-p4-sync"
set "SCRIPT_BASH=%SCRIPT_WIN:\=/%"
set "SCRIPT_BASH=/%SCRIPT_BASH:~0,1%%SCRIPT_BASH:~2%"

%BASH_PATH% --login -c "%SCRIPT_BASH%"
set "RC=%ERRORLEVEL%"

exit /b %RC%
