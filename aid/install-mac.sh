#!/usr/bin/env sh
# -*- coding: urf-8; mode: shell-script -*-

# untested script, I feel it should work though...

# Confirm current platform to be MacOS
confirm_mac () {
    ostype="$(uname -s)"
    printf "detected system: %s.\n" "${ostype}"
    case "${ostype}" in
        Darwin|darwin)
            printf "we shall attempt an automated installation.\n"
            printf "although, you may be asked questions by 'Homebrew'.\n"
            ;;
        Linux|linux)
            printf "You don't need to be spoon-fed.\n"
            printf "Check Documentation. %s\n"\
                   "https://pradyparanjpe.gitlab.io/pspmacs/index.html"
            exit 66
            ;;
        *)
            printf "Currently supported: GNU/Linux and Darwin (MacOS)\n" >&2
            exit 66
            ;;
    esac
    unset ostype
}

# install homebrew, the missing package manager
# as prescribed here: https://brew.sh/
install_homebrew () {
    printf "\n\n"
    if builtin command -v "brew"; then
        printf "found that brew is already installed.\n"
    else
        printf "installing Homebrew, the missing package manager.\n"
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
}

# install Emacs
# ref: https://www.emacswiki.org/emacs/EmacsForMacOS
install_emacs () {
    printf "\n\n"
    printf "installing Emacs using homebrew.\n"
    brew install emacs
    brew install --cask emacs
}

# install fonts:
# - FiraCode: https://github.com/tonsky/FiraCode/wiki/Installing
# - ViktorMono: https://rubjo.github.io/victor-mono/
install_fonts () {
    printf "\n\n"
    printf "installing fonts using homebrew.\n"
    brew tap homebrew/cask-fonts
    brew install --cask font-fira-code
    brew install --cask font-victor-mono
}

# dependencies: git gcc coreutils make autoconf node ripgrep stow gnupg
install_dependencies () {
    printf "\n\n"
    printf "trying to install Gnu/Linux system-dependencies using homebrew.\n"
    brew install git gcc coreutils make autoconf node ripgrep stow gnupg
}

# Back up existing Emacs from standard locations. with a .bak extension
backup_std_emacs () {
    printf "\n\n"
    printf "backing up standard Emacs locations to '<filename>.bak'."
    if [ -f "${HOME}/.emacs.el" ]; then
        mv "${HOME}/.emacs.el" "${HOME}/.emacs.el.bak"
    fi
    for el_loc in "${XDG_CONFIG_HOME:-${HOME}/.config}/emacs" \
                      "${HOME}/.emacs" \
                      "${HOME}/.emacs.d/init.el"; do
        if [ -d "${el_loc}" ]; then
            mv "${el_loc}" "${el_loc}.bak"
        fi
    done

    ln -sf "${XDG_DATA_HOME:-${HOME}/.local/share}/emacs/pspmacs" \
       "${XDG_CONFIG_HOME:-${HOME}/.config}/emacs"
    ln -sf "${XDG_DATA_HOME:-${HOME}/.local/share}/emacs/pspmacs" \
       "${HOME}/.emacs.d"
}

# Clone pspmacs installation
clone_pspmacs () {
    printf "\n\n"

    # set environment
    printf "Preparing environment.\n"

    # XDG_DATA_HOME/emacs
    emacs_data="${XDG_DATA_HOME:-${HOME}/.local/share}/emacs"


    # XDG_CACHE_HOME/emacs
    emacs_cache="${XDG_CACHE_HOME:-${HOME}/.cache}/emacs"

    # XDG_STATE_HOME/emacs
    emacs_state="${XDG_STATE_HOME:-${HOME}/.local/state}/emacs"

    mkdir -p "${emacs_data}/src/"
    mkdir -p "${emacs_data}/pspmacs/"
    mkdir -p "${emacs_cache}/local.d/packages/gnupg"
    mkdir -p "${emacs_state}/"
    gpg --homedir "${LOCAL_EMACS_HOME}/packages/gnupg" \
        --receive-keys "066DAFCB81E42C40"

    # Permenant export (We assume zsh (new) or bash (old) shell)
    printf "\n\nexport LOCAL_EMACS_HOME=\"%s/local.d/\"\n" \
           "${emacs_cache}" >> "${HOME}/.bashrc"
    printf "\n\nexport LOCAL_EMACS_HOME=\"%s/local.d/\"\n" \
           "${emacs_cache}" >> "${HOME}/.zshrc"

    # session export
    export LOCAL_EMACS_HOME="${emacs_cache}/local.d/"

    printf "cloning PSPMACS.\n"
    git clone --recurse-submodules \
        "https://www.gitlab.com/pradyparanjpe/pspmacs.git" \
        "${emacs_data}/pspmacs"

    unset emacs_data emacs_cache emacs_state
}

# Print some information and instructions
printf_byelogue () {
    bye_message="
    [INFO] Emacs is installed and PSPMacs cloned.

    [ACT]  Launch Emacs to (auto) install necessary packages.

    [WARN] Depending on internet speed, CPU and storage capabilities,
           first launch may take more than a minute, may be much much more.
    [ACT]  Emacs will ask easy 'yes-or-no' questions.

    [ACT]  Launch (and Close) Emacs multiple times, till
           a 'sub-window' with error(s) no more pops up.
    [ERR]  If such error(s) pop(s) up even after 5 launches, something's wrong.

    [WISH] Best!

    --
    Prady
"
    printf "%s\n" "${bye_message}"
    unset bye_message
}

# initialize pspmacs
main () {
    confirm_mac || exit 65
    install_homebrew || exit 65
    install_fonts || exit 65
    install_dependencies || exit 65
    install_emacs || exit 65
    clone_pspmacs || exit 65
    backup_std_emacs
    printf_byelogue
}

main "$@"
