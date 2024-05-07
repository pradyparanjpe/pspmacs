$emacs_data=("$env:LOCALAPPDATA" + "\emacs")
$emacs_state=("$env:TEMP" + "\emacs")

function backup_std_emacs {
    Write-Host "`r`n"
    Write-Host "[PART] Back-up"
    Write-Host "[INFO] backing up '.emacs.d' locations to '.emacs.d.bak'."
    move-Item -Force -ErrorAction SilentlyContinue `
      ("$env:userprofile" + "\.emacs.d") ("$env:userprofile" + "\.emacs.d.bak")
    move-Item -Force -ErrorAction SilentlyContinue `
      ("$env:appdata" + "\.emacs.d") ("$env:appdata" + "\.emacs.d.bak")
}

function set_local_home {
    Write-Host "[INFO] Setting a local cache for Emacs packages."
    if ("${LOCAL_EMACS_HOME}") {
        Write-Host "[INFO] LOCAL_EMACS_HOME is set to %s, using it.`r`n" `
          "${LOCAL_EMACS_HOME}"
        return
    }

    $LOCAL_EMACS_HOME=("$emacs_data" + "\local.d")
    New-Item "$LOCAL_EMACS_HOME\packages\gnupg" -Force -ItemType Directory
    gpg --homedir "$LOCAL_EMACS_HOME\packages\gnupg" `
      --receive-keys "066DAFCB81E42C40"
    [System.Environment]::SetEnvironmentVariable(
        'LOCAL_EMACS_HOME, $LOCAL_EMACS_HOME, 'User'
    )
}

function clone_pspmacs {
    Write-Host "`r`n"
    Write-Host "[PART] Download PSPMacs"

    # set environment
    Write-Host "[INFO] Preparing environment."

    New-Item ("$emacs_data" + "\pspmacs") -Force -ItemType Directory
    New-Item "$emacs_state" -Force -ItemType Directory

    Write-Host "[INFO] cloning PSPMacs."
    git clone --recurse-submodules `
      "https://www.gitlab.com/pradyparanjpe/pspmacs.git" `
      ("$emacs_data\" + "pspmacs")
}

function set_emacs_config {
    Write-Host "[PART] Infecting Emacs with pspmacs config."
    cmd \c mklink \d ("$env:userprofile" + "\.emacs.d") `
      ("$emacs_data" + "\pspmacs")
    cmd \c mklink \d ("$env:appdata" + "\.emacs.d") `
      ("$emacs_data" + "\pspmacs")
}

function main {
    clone_pspmacs
    backup_std_emacs
    set_local_home
    set_emacs_config
}
main
