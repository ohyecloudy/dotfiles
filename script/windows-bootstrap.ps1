$script_loc = $MyInvocation.MyCommand.Path.Replace($MyInvocation.MyCommand.Name, "")
Write-Host "script directory - " $script_loc

[string]$choco = $env:ChocolateyInstall
if ($choco -eq "")
{
    # Chocolatey 설치 여부를 ChocolateyInstall 환경 변수가 있는지 검사해서 알아낸다.
    Write-Host "install chocolatey"
    
    iex ((new-object net.webclient).DownloadString("http://bit.ly/psChocInstall"))    
}

Write-Host "update chocolatey"
cup chocolatey

Write-Host "update packages"
cup vim
cup emacs
cup python
cup Wget
cup Lein

