@REM https://gist.github.com/roundand/9367852
@chcp 65001

SET CUR_EMACS_BIN_DIR
if "%CUR_EMACS_BIN_DIR%"=="" (
    echo "Cannot find `CUR_EMACS_BIN_DIR` environment variable"
    set errorlevel=4
    goto :finally
)

call is-elevated.bat || goto :finally

SET ecPath=%%CUR_EMACS_BIN_DIR%%\emacsclientw.exe

REG ADD "HKCR\*\shell\Open with Emacs"         /t REG_SZ /v "" /d "Open with Emacs" /f || goto :finally
REG ADD "HKCR\*\shell\Open with Emacs"         /t REG_EXPAND_SZ /v "Icon" /d "%ecPath%,0" /f || goto :finally
REG ADD "HKCR\*\shell\Open with Emacs\command" /t REG_EXPAND_SZ /v "" /d "%ecPath% \"%%1\"" /f || goto :finally

REG ADD "HKCR\Folder\shell\Open with Emacs"         /t REG_SZ /v "" /d "Open with Emacs" /f || goto :finally
REG ADD "HKCR\Folder\shell\Open with Emacs"         /t REG_EXPAND_SZ /v "Icon" /d "%ecPath%,0" /f || goto :finally
REG ADD "HKCR\Folder\shell\Open with Emacs\command" /t REG_EXPAND_SZ /v "" /d "%ecPath% \"%%1\"" /f || goto :finally

:finally
@set err=%errorlevel%

@echo off
if %err% neq 0 (
   echo.
   echo ▄████████████▄▐█▄▄▄▄█▌
   echo █████▌▄▌▄▐▐▌██▌▀▀██▀▀
   echo ███▄█▌▄▌▄▐▐▌▀██▄▄█▌
   echo ▄▄▄▄█████████████
   echo.
)
@echo on

exit /b %err%
