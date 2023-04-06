#!/usr/bin/env sh
# -*- coding: urf-8; mode: shell-script -*-

# untested script, I feel it should work though...

# set script-variables
set_vars () {
    # directory locations used by Emacs to fetch init files.
    emacs_init_dirs="${XDG_CONFIG_HOME:-${HOME}/.config}/emacs \
                    ${HOME}/.emacs \
                    ${HOME}/.emacs.d/init.el"

    # File locaitions used by Emacs as init files.
    emacs_init_files="${HOME}/.emacs.el"
}

# unset script-variables
unset_vars () {
    unset emacs_init_dirs emacs_init_files
}

# Confirm current platform to be MacOS
confirm_mac () {
    ostype="$(uname -s)"
    printf "[INFO] detected system: %s.\n" "${ostype}"
    case "${ostype}" in
        Darwin|darwin)
            printf "[INFO] we shall attempt an automated installation.\n"
            printf "[ACT]  you may be asked questions by 'Homebrew'.\n"
            ;;
        Linux|linux)
            printf "[DENY] You don't need to be spoon-fed.\n"
            printf "[ACT]  Check Documentation. %s\n"\
                   "https://pradyparanjpe.gitlab.io/pspmacs/index.html"
            exit 66
            ;;
        *)
            printf "[FAIL] Currently supported: GNU/Linux and Darwin (MacOS)\n" >&2
            exit 66
            ;;
    esac
    unset ostype
}

# install homebrew, the missing package manager
# as prescribed here: https://brew.sh/
install_homebrew () {
    printf "\n\n"
    printf "[PART] Homebrew:\n"
    if builtin command -v "brew"; then
        printf "[INFO] found that brew is already installed.\n"
    else
        printf "[INFO] installing Homebrew, the missing package manager.\n"
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
}

# install Emacs
# ref: https://www.emacswiki.org/emacs/EmacsForMacOS
install_emacs () {
    printf "\n\n"
    printf "[PART] Emacs:\n"
    printf "[INFO] installing Emacs using homebrew.\n"
    brew install emacs
    brew install --cask emacs
}

# install fonts:
# - FiraCode: https://github.com/tonsky/FiraCode/wiki/Installing
# - ViktorMono: https://rubjo.github.io/victor-mono/
install_fonts () {
    printf "\n\n"
    printf "[PART] Fonts:\n"
    printf "[INFO] installing fonts using homebrew.\n"
    brew tap homebrew/cask-fonts
    brew install --cask font-fira-code
    brew install --cask font-victor-mono
}

# dependencies: git gcc coreutils make autoconf node ripgrep stow gnupg
install_dependencies () {
    printf "\n\n"
    printf "[PART] GNU/Linux dependencies:\n"
    printf "[INFO] trying to install Gnu/Linux system-dependencies using homebrew.\n"
    brew install git gcc coreutils make autoconf \
         node ripgrep stow gnupg diction
}

# Back up existing Emacs from standard locations. with a .bak extension
backup_std_emacs () {
    printf "\n\n"
    printf "[PART] Back-up:\n"
    printf "[INFO] backing up standard Emacs locations to '<location>.bak'."
    for el_loc in ${emacs_init_dirs}; do
        if [ -d "${el_loc}" ]; then
            mv "${el_loc}" "${el_loc}.bak"
        fi
    done

    for el_loc in ${emacs_init_files}; do
        if [ -f "${el_loc}" ]; then
            mv "${el_loc}" "${el_loc}.bak"
        fi
    done
}

# softlink ~/.config/emacs and ~/.emacs.d to ~/.local/share/emacs/pspmacs
set_emacs_config () {
    ln -sf "${XDG_DATA_HOME:-${HOME}/.local/share}/emacs/pspmacs" \
       "${XDG_CONFIG_HOME:-${HOME}/.config}/emacs" || revert_backup

    ln -sf "${XDG_DATA_HOME:-${HOME}/.local/share}/emacs/pspmacs" \
       "${HOME}/.emacs.d" || revert_backup
}

# reset backups to original
revert_backup () {
    printf "\n\n"
    printf "[FAIL] Failed linking configuration files.\n"
    printf "[INFO] Reverting from Back-up:\n"
    for el_loc in ${emacs_init_dirs}; do
        if [ -d "${el_loc}.bak" ]; then
            mv "${el_loc}.bak" "${el_loc}"
        fi
    done

    for el_loc in ${emacs_init_files}; do
        if [ -f "${el_loc}.bak" ]; then
            mv "${el_loc}.bak" "${el_loc}"
        fi
    done
    printf "[FAIL] Automated set-up failed.\n"
    unset_vars
    exit 1
}

# Clone pspmacs installation
clone_pspmacs () {
    printf "\n\n"
    printf "[PART] Download PSPMacs:\n"

    # set environment
    printf "[INFO] Preparing environment.\n"

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

    printf "[INFO] cloning PSPMacs.\n"
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
    set_vars
    confirm_mac || exit 65
    install_homebrew || exit 65
    install_fonts || exit 65
    install_dependencies || exit 65
    install_emacs || exit 65
    clone_pspmacs || exit 65
    backup_std_emacs
    set_emacs_config
    unset_vars
    printf_byelogue
}

main "$@"
