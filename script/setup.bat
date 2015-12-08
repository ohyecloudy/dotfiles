@pushd %cd%
@chcp 65001

@set hpath=%homedrive%%homepath%
@set dotpath=%~dp0..
@set binpath=%dotpath%\bin

call %binpath%\is-elevated.bat || goto :finally

setx HOME %hpath% || goto :finally

mklink %hpath%\.gitignore_global %dotpath%\git\gitignore_global || goto :finally
mklink %hpath%\.gitconfig %dotpath%\git\gitconfig || goto :finally
mklink %hpath%\.vimrc %dotpath%\vim\vimrc || goto :finally

mklink /J %hpath%\.emacs.d %dotpath%\emacs.d || goto :finally

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
