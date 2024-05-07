if (-NOT ([Security.Principal.WindowsPrincipal]
          [Security.Principal.WindowsIdentity]::GetCurrent())
    .IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator"))
{
    $arguments = "& '" +$myinvocation.mycommand.definition + "'"
    Start-Process powershell -Verb runAs -ArgumentList $arguments
    Break
}

Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = `
  [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
iex ((New-Object System.Net.WebClient)
     .DownloadString('https://community.chocolatey.org/install.ps1'))

choco install -y gnupg gnuwin32-coreutils.install mingw git.install `
  curl zip nodejs ripgrep

choco install -y nerd-fonts-firacode nerd-fonts-victormonoo

choco install -y emacs
