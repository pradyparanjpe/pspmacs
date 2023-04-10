#!/usr/bin/env sh
# -*- coding: urf-8; mode: shell-script -*-
# Copyright © 2023 Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
# Clone and initialize PSPMacs (POSIX)
# tested to complete on alpine Linux in gitlab-ci runner


#########################
#                       #
#  GNU/LINUX FUNCTIONS  #
#                       #
#########################


# Initialize package manager for the specific distribution
#
# Args:
#     NULL
# Returns:
#     NULL
linux_init_package_manager () {
    package_managers="apk apt dnf pacman zypper"
    printf "\n\n"
    printf "[PART] guessing and initializing the package manager.\n"
    for manager in $package_managers; do
        if command -v "${manager}" >/dev/null 2>&1; then
            printf "[INFO] initializing package manager '%s'\n." "${manager}"
            package_"${manager}"_initialize
            package_install () {
                package_"${manager}"_install "$@"
            }
            return
        fi
    done
    clean_exit 1 "Supported package managers: ${package_managers}."
}


# install fonts locally:
# - FiraCode: https://github.com/tonsky/FiraCode/wiki/Installing
# - ViktorMono: https://rubjo.github.io/victor-mono/
linux_install_fonts () {
    # local fonts directory
    printf "[PART] Fonts\n"
    printf "[INFO] installing fonts locally.\n"

    if ! command -v fc-cache >/dev/null 2>&1; then
        package_install fontconfig
    fi

    fonts_dir="${XDG_DATA_HOME:-${HOME}/.local/share}/fonts"
    fira_url="https://github.com/tonsky/FiraCode/releases\
/download/5.2/Fira_Code_v5.2.zip"
    viktor_url="https://rubjo.github.io/victor-mono/VictorMonoAll.zip"

    if [ ! -d "${fonts_dir}" ]; then
        mkdir -p "${fonts_dir}"
    fi

    for url in "${fira_url}" "${viktor_url}"; do
        curl --fail --location --show-error "${url}" --output "font_file.zip" \
            || clean_exit 65 "couldn't download %s.\n" "${url}"
        unzip -o -q -d "${fonts_dir}" "font_file.zip"
        rm "font_file.zip"
    done

    echo "fc-cache -f"
    fc-cache -f || clean_exit 65 "couldn't initialize fonts.\n"
    unset fonts_dir fira_url viktor_url
}


package_apk_initialize () {
    # generally in a container with root
    apk update || clean_exit 65 "[APK]  Couldn't update."
    # sudo apk update || clean_exit 65 "[APK]  Couldn't update."
}


package_apt_initialize () {
    sudo apt update || clean_exit 65 "[APT]  Couldn't update."
}


package_dnf_initialize () {
    sudo dnf -y update || clean_exit 65 "[DNF]  Couldn't update."
}


package_pacman_initialize () {
    true
}


package_zypper_initialize () {
    sudo zypper ref || clean_exit 65 "[ZYPR]  Couldn't update."
}


package_apk_install () {
    apk add "$@" || clean_exit 65 "[APK]  Couldn't install $*."
    # sudo apk update || clean_exit 65 "[APK]  Couldn't update."
}


package_apt_install () {
    sudo apt install -y "$@" || clean_exit 65 "[APT]  Couldn't install $*."
}


package_dnf_install () {
    sudo dnf -y install "$@" || clean_exit 65 "[DNF]  Couldn't install $*."
}


package_pacman_install () {
    sudo pacman --noconfirm -Syu "$@" \
        || clean_exit 65 "[ARCH]  Couldn't install $*."
}


package_zypper_install () {
    sudo zypper -n install "$@" || clean_exit 65 "[ZYPR]  Couldn't install $*."
}


#######################
#                     #
#   MacOS FUNCTIONS   #
#                     #
#######################


# Initialize package manager for the specific MacOS: homebrew
# install homebrew, the missing package manager
# as prescribed here: https://brew.sh/
#
# Args:
#     NULL
# Returns:
#     NULL
macos_init_package_manager () {
    printf "\n\n"
    printf "[PART] Homebrew\n"
    if command -v "brew"; then
        printf "[INFO] found that brew is already installed.\n"
    else
        printf "[INFO] installing Homebrew, the missing package manager.\n"
        printf "[ACT]  You may be asked questions by 'Homebrew'.\n"
        /bin/bash -c "$(curl -fsSL \
https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    brew tap homebrew/cask-fonts
    package_install () {
        macos_package_install "$@"
    }
}


# Installation wrapper around default package manager
#
# Args:
#     NULL
# Returns:
#     NULL
macos_package_install () {
    deps="$*"
    if [ ! "${deps}" = "${deps#npm*}" ]; then
        # npm is in deps
        if [ "${deps}" = "npm" ]; then
            # npm is the only dependency - argument passed
            deps="node"
        elif [ "${deps}" = "${deps#npm}" ]; then
             # npm is not the first dep
             deps="${deps% npm*} node${deps##* npm}"
        else
            # npm is the first dep
             deps="${deps%npm *}node ${deps##*npm }"
        fi
    fi
    brew install "$@"
    # What about cask?
}


# Install fonts
# - FiraCode: https://github.com/tonsky/FiraCode/wiki/Installing
# - ViktorMono: https://rubjo.github.io/victor-mono/
macos_install_fonts () {
    printf "[INFO] installing fonts using homebrew.\n"
    brew install --cask font-fira-code font-victor-mono
}


#########################
#                       #
#  SCRIPT ENVIRONMENT   #
#                       #
#########################


# set script-variables
set_vars () {
    # directory locations used by Emacs to fetch init files.
    emacs_init_dirs="${XDG_CONFIG_HOME:-${HOME}/.config}/emacs \
                    ${HOME}/.emacs \
                    ${HOME}/.emacs.d/init.el"

    # File locaitions used by Emacs as init files.
    emacs_init_files="${HOME}/.emacs.el"

    # XDG_CONFIG_HOME/emacs
    emacs_config="${XDG_CONFIG_HOME:-${HOME}/.config}/emacs"

    # XDG_DATA_HOME/emacs
    emacs_data="${XDG_DATA_HOME:-${HOME}/.local/share}/emacs"

    # XDG_CACHE_HOME/emacs
    emacs_cache="${XDG_CACHE_HOME:-${HOME}/.cache}/emacs"

    # XDG_STATE_HOME/emacs
    emacs_state="${XDG_STATE_HOME:-${HOME}/.local/state}/emacs"

    # OS
    ostype="$(uname -s)"

    # OS dependencies
    dependencies="gcc make autoconf git curl zip npm ripgrep stow emacs"

    # variables to unset
    global_vars="emacs_init_dirs emacs_init_files emacs_config emacs_data \
                                 emacs_cache emacs_state dependencies ostype"
}


# unset script-variables
unset_vars () {
    # shellcheck disable=SC2086
    unset ${global_vars}
}


# Clean environment and exit optionally with an error message
#
# Args:
#     $1: exit error code
#     $2: error / exit message
clean_exit() {
    unset_vars
    if [ -n "${1}" ] && [ "${1}" -ne "0" ]; then
        printf "[FAIL] Automated installation failed.\n"
        printf "[ACT]  Check Documentation. %s\n"\
               "https://pradyparanjpe.gitlab.io/pspmacs/index.html"
        if [ -n "${2}" ]; then
            # shellcheck disable=SC2059
            printf "${2}\n" >&2
        fi
        # shellcheck disable=SC2086
        unset_vars
        exit "${1}"
    fi
    if [ -n "${2}" ]; then
        # shellcheck disable=SC2059
        printf "${2}\n"
    fi
    unset_vars
    exit 0
}


#########################
#                       #
#    SCRIPT SEGMENTS    #
#                       #
#########################


# Detect OS platform
detect_os () {
    printf "[INFO] detected system: %s.\n" "${ostype}"
    case "${ostype}" in
        [Ll]inux)
            printf "[INFO] We shall attempt an automated installation.\n"
            printf "[TUX]  Automated installation for GNU/Linux.\n"
            printf "[INFO] loading GNU/Linux functions.\n"
            linux_init_package_manager || clean_exit 65
            install_fonts () {
                linux_install_fonts
            }
            ;;
        [Dd]arwin)
            printf "[INFO] We shall attempt an automated installation.\n"
            printf "[MAC]  Automated installation on Apple: MacOS.\n"
            printf "[INFO] loading Mac functions.\n"
            macos_init_package_manager || clean_exit 65
            install_fonts () {
                macos_install_fonts
            }
            ;;
        *)
            clean_exit 66 "[FAIL] Currently supported: GNU/Linux and Darwin (MacOS)\n"
            ;;
    esac
}


# dependencies
install_dependencies () {
    printf "\n\n"
    printf "[PART] GNU/Linux dependencies\n"
    printf "[INFO] trying to install GNU/Linux system-dependencies.\n"
    printf "[ACT]  you may be asked questions by the package-manager.\n"
    printf "[INFO] Installing coreutils, gnupg\n"
    package_install "coreutils" "gnupg"
    for dep in ${dependencies}; do
        if ! command -v "${dep}" >/dev/null 2>&1; then
            printf "[INFO] Installing %s\n" "${dep}"
            package_install "${dep}"
        fi
    done
}


# Back up existing Emacs from standard locations. with a .bak extension
# This function is common to all POSIX
backup_std_emacs () {
    printf "\n\n"
    printf "[PART] Back-up\n"
    printf "[INFO] backing up standard Emacs locations to '<location>.bak'.\n"
    for el_loc in ${emacs_init_dirs}; do
        if [ -d "${el_loc}" ]; then
            mv "${el_loc}" "${el_loc}.bak" >/dev/null 2>&1 || true
        fi
    done

    for el_loc in ${emacs_init_files}; do
        if [ -f "${el_loc}" ]; then
            mv "${el_loc}" "${el_loc}.bak" >/dev/null 2>&1 || true
        fi
    done
}


# Clone pspmacs installation
# This function is common to all POSIX
clone_pspmacs () {
    printf "\n\n"
    printf "[PART] Download PSPMacs\n"

    # set environment
    printf "[INFO] Preparing environment.\n"

    mkdir -p "${emacs_data}/src/"
    mkdir -p "${emacs_data}/pspmacs/"
    mkdir -p "${emacs_state}/"

    printf "[INFO] cloning PSPMacs.\n"
    git clone --recurse-submodules \
        "https://www.gitlab.com/pradyparanjpe/pspmacs.git" \
        "${emacs_data}/pspmacs"
}


# Location fo $LOCAL_EMACS_HOME
set_local_home () {
    printf "[INFO] Setting a local cache for Emacs packages.\n"
    if [ -n "${LOCAL_EMACS_HOME}" ]; then
        printf "[INFO] LOCAL_EMACS_HOME is set to %s, using it.\n" \
               "${LOCAL_EMACS_HOME}"
        return
    fi

    LOCAL_EMACS_HOME="${emacs_cache}/.local.d"
    printf "Do you want to set local cache in %s? [y/N]\t" "${LOCAL_EMACS_HOME}"
    read -r yn

    case "${yn}" in
        [Yy]*)
            rc_export="\
\n\n# ADDED BY PSPMACS\n\
LOCAL_EMACS_HOME=\"${LOCAL_EMACS_HOME}\"\n\
export LOCAL_EMACS_HOME\n"
            printf "%s\n" "${rc_export}" > "${HOME}/.bashrc"
            printf "%s\n" "${rc_export}" > "${HOME}/.zshrc"
            ;;
        *)
            LOCAL_EMACS_HOME="${PVT_EMACS_HOME:-${emacs_data}/pspmacs}/local.d"
            ;;
    esac
    export LOCAL_EMACS_HOME

    mkdir -p "${LOCAL_EMACS_HOME}/packages/gnupg"
    gpg --homedir "${LOCAL_EMACS_HOME}/packages/gnupg" \
        --receive-keys "066DAFCB81E42C40"
}


# softlink ~/.config/emacs and ~/.emacs.d to ~/.local/share/emacs/pspmacs
# This function is common to all POSIX
set_emacs_config () {
    printf "[PART] infecting Emacs with pspmacs config.\n"
    mkdir -p "${emacs_config}"
    ln -sf "${emacs_config}" "${HOME}/.emacs.d" || revert_backup
    ln -sf "${emacs_data}/pspmacs" "${emacs_config}" || revert_backup
}


# reset backups to original
# This function is common to all POSIX
revert_backup () {
    printf "\n\n"
    printf "[FAIL] Failed linking configuration files.\n"
    printf "[INFO] Reverting from Back-up:\n"
    for el_loc in ${emacs_init_dirs}; do
        if [ -d "${el_loc}.bak" ]; then
            mv "${el_loc}.bak" "${el_loc}" >/dev/null 2>&1 || true
        fi
    done

    for el_loc in ${emacs_init_files}; do
        if [ -f "${el_loc}.bak" ]; then
            mv "${el_loc}.bak" "${el_loc}" >/dev/null 2>&1 || true
        fi
    done
    printf "[FAIL] Automated set-up failed.\n"
    unset_vars
    clean_exit 66
}


# Print some information and instructions
# This function is common to all POSIX
printf_byelogue () {
    bye_message="



    [INFO] Emacs is installed and PSPMacs cloned.

    [ACT]  Launch Emacs to (auto) install necessary Emacs packages.

    [WARN] Depending on internet speed, CPU and storage capabilities,
           first launch may take more than a minute, may be much much more.
    [ACT]  Emacs will ask easy 'yes-or-no' questions.

    [ACT]  Launch (and Close) Emacs multiple times, till
           a 'sub-window' with error(s) no more pops up.
    [KEYS] To close Emacs, use the key-sequence 'Ctrl+x Ctrl+c'
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
    detect_os || clean_exit 65
    install_dependencies || clean_exit 65
    install_fonts || clean_exit 65
    clone_pspmacs || clean_exit 65
    backup_std_emacs
    set_local_home
    set_emacs_config
    unset_vars
    printf_byelogue
}


main "$@"
