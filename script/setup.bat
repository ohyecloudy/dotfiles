@pushd %cd%
@chcp 65001

@set hpath=%homedrive%%homepath%
@set binpath=%~dp0..\bin

call %binpath%\is-elevated.bat || goto :finally

setx HOME %hpath% || goto :finally

mklink %hpath%\.gitignore_global %hpath%\.dotfiles\git\gitignore_global || goto :finally
mklink %hpath%\.gitconfig %hpath%\.dotfiles\git\gitconfig || goto :finally
mklink %hpath%\.vimrc %hpath%\.dotfiles\vim\vimrc || goto :finally

mklink /J %hpath%\.emacs.d %hpath%\.dotfiles\emacs.d || goto :finally

:finally
@set err=%errorlevel%

@popd

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
