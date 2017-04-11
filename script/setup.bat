@pushd %cd%
@chcp 65001

@set hpath=%homedrive%%homepath%
@set dotpath=%~dp0..
@set binpath=%dotpath%\bin

call %binpath%\is-elevated.bat || goto :finally

echo ;%path%; | find /c /i ";%hpath%\bin.local;" > NUL || setx /m PATH "%PATH%;%hpath%\bin.local;%hpath%\bin"
setx /m HOME %hpath% || goto :finally

mklink %hpath%\.gitignore_global %dotpath%\git\gitignore_global || goto :finally
mklink %hpath%\.gitconfig %dotpath%\git\gitconfig || goto :finally
mklink %hpath%\.vimrc %dotpath%\vim\vimrc || goto :finally
mklink %hpath%\.bashrc %dotpath%\bashrc || goto :finally
mklink %hpath%\.bash_profile %dotpath%\bash_profile || goto :finally

mklink /J %hpath%\.emacs.d %dotpath%\emacs.d || goto :finally
mklink /J %hpath%\bin %dotpath%\bin || goto :finally
mklink /J %hpath%\bin.local %dotpath%\bin.local || goto :finally

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
