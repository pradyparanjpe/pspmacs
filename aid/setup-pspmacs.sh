#!/usr/bin/env sh
# -*- coding: utf-8; mode: shell-script; -*-
#
# Copyright © 2023 Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
#
# Author: Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
# Keywords: help, languages

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# Clone and initialize PSPMacs (POSIX)
# tested to complete on alpine Linux in gitlab-ci runner

###############################################################################

#########################
#                       #
#  SCRIPT ENVIRONMENT   #
#                       #
#########################

# set script-variables
#
# Args:
#     NULL
# Returns:
#     NULL
set_vars () {
    # OS
    ostype="$(uname -s)"
    if [ "${ostype%inux}" = "${ostype}" ]; then
        manager="homebrew"
    elif [ "${ostype%arwin}" = "${ostype}" ]; then \
        # package manager to use
        manager=
    else
        clean_exit 66 \
                   "[FAIL] Currently supported: GNU/Linux and Darwin (MacOS)\n"
    fi

    # coded package manager
    package_managers="apk apt dnf pacman zypper"

    # install dependencies
    include_deps=true

    # recommended dependences
    recommend="
    shell-utilities: sed awk
    package-managers: pip
    linters: shellcheck bashate pylint diction
    code-structurers: isort yapf
    exporters: latex xetex pandoc
"

    # ?install fonts
    include_fonts=true


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

    # OS dependencies
    dependencies="gcc make autoconf git curl zip npm ripgrep stow emacs"

    # fonts plist
    # format: one font-entry per line (delimited by $IFS)
    # font entry-format:
    # <brew-installation-suffix>=<font-release.zip>
    # brew-installation-suffix is installation term dropping the "font-" prefix
    # font-release should correctly unzip to necessary format in ~/.local/share/fonts
    fonts="fira-code=https://github.com/tonsky/FiraCode/releases\
/download/5.2/Fira_Code_v5.2.zip
victor-mono=https://rubjo.github.io/victor-mono/VictorMonoAll.zip
fira-code-nerd=https://github.com/ryanoasis/nerd-fonts/releases\
/download/v3.0.2/FiraCode.zip"

    # variables to unset
    global_vars="help_msg usage manager include_deps include_fonts emacs_cache\
                          emacs_init_files emacs_data emacs_state emacs_config\
                          emacs_init_dirs dependencies package_managers ostype
                          fonts"

    # usage message
    usage="
    usage:
    ${0} -h
    ${0} --help
    ${0} [[optional arguments] ...]
"
    help_msg="${usage}

    DESCRIPTION:
    Set up PSPMacs for ${ostype}

    TIP:
    Check documentation: https://pradyparanjpe.gitlab.io/pspmacs/index.html to
    decide if following environment variables need to be set.
    Current values:
    LOCAL_EMACS_HOME='${LOCAL_EMACS_HOME:-<BLANK>}'
    PVT_EMACS_HOME='${PVT_EMACS_HOME:-<BLANK>}'

    Optional Arguments:
    -h\t\t\tprint usage message and exit
    --help\t\tprint this message and exit
    -d|--no-deps\tDon't install dependencies (skip to the next PART).
    -F|--no-fonts\tDon't install fonts (skip to the next PART).
    --recommend\t\tShow a lisf of recommended dependencies.
"
    if [ ! "${ostype%inux}" = "${ostype}" ]; then
        help_msg="${help_msg}    --manager MAN\tUse MAN as the package manager.
    --list-deps\t\tList dependencies installed by this script.
    --list-fonts\tList fonts installed by this script.

    Supported package managers: ${package_managers}
    for others, ensure dependencies yourself and then run with --no-deps.
"
        recommend="${recommend}    clipboard: wl-clipboard xcopy
"
    fi

}

# unset script-variables
#
# Args:
#     NULL
# Returns:
#     NULL
unset_vars () {
    # shellcheck disable=SC2086
    unset ${global_vars}
}

# Clean environment and exit optionally with an error message
#
# Args:
#     $1: exit error code
#     $2: error / exit message
#
# Returns:
#     NULL
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

# command line arguments
#
# Args:
#     Command line arguments, usually "$@"
# Returns:
#     NULL
cli () {
    while test $# -gt 0; do
        case $1 in
            -h)
                clean_exit 0 "${usage}"
                ;;
            --help)
                clean_exit 0 "${help_msg}"
                ;;
            --recommend)
                clean_exit 0 "
Recommended Dependencies:
    ${recommend}
"
                ;;
            --list-deps)
                clean_exit 0 "
Dependencies:
    ${dependencies}"
                ;;
            --list-fonts)
                clean_exit 0 "
Fonts:

${fonts}
"
                ;;
            -d|--no-deps)
                include_deps=false
                shift
                ;;
            -F|--no-fonts)
                include_fonts=false
                shift
                ;;
            --manager|--manager=*)
                if [ ! "${1#*=}" = "${1}" ]; then
                    manager="$(echo "$1" | cut -d "=" -f 2)"
                else
                    shift
                    manager="${1}"
                fi
                shift
                if [ "${package_managers#*"${manager}"}" = "${package_managers}" ]; then
                    clean_exit 1 "Supported package managers: ${package_managers}."
                fi
                ;;
            *)
                clean_exit 1 "${usage}"
                ;;
        esac
    done
}

###############################################################################

#########################
#                       #
#  GNU/LINUX FUNCTIONS  #
#                       #
#########################

# Install fonts locally
#
# Args:
#     NULL
# Returns:
#     NULL
linux_install_fonts () {
    # local fonts directory
    printf "[INFO] installing fonts locally.\n"

    if ! command -v fc-cache >/dev/null 2>&1; then
        package_install fontconfig
    fi

    fonts_dir="${XDG_DATA_HOME:-${HOME}/.local/share}/fonts"

    if [ ! -d "${fonts_dir}" ]; then
        mkdir -p "${fonts_dir}"
    fi

    for entry in ${fonts}; do
        font_name="$(echo "${entry}" | cut -d= -f1)"
        url="$(echo "${entry}" | cut -d= -f2)"
        printf "[INFO] Installing %s, downloading from %s." \
               "${font_name}" "${url}"
        curl --fail --location --show-error "${url}" \
             --output "${font_name}.zip" \
            || clean_exit 65 "couldn't download %s.\n" "${url}"
        unzip -o -q -d "${fonts_dir}" "${font_name}.zip"
        rm "${font_name}.zip"
    done

    echo "[INFO] Rebuilding local fonts cache."
    fc-cache -f || clean_exit 65 "Couldn't initialize fonts.\n"
    unset fonts_dir
}

# Wrapper around apk initialization (Alpine Linux and derivatives)
#
# Args:
#     NULL
# Returns:
#     NULL
package_apk_initialize () {
    # generally in a container with root
    # shellcheck disable=SC2317
    apk update || clean_exit 65 "[APK]  Couldn't update."
    # sudo apk update || clean_exit 65 "[APK]  Couldn't update."
}

# Wrapper around apt initialization (Debian Linux and derivatives)
#
# Args:
#     NULL
# Returns:
#     NULL
package_apt_initialize () {
    # shellcheck disable=SC2317
    sudo apt update || clean_exit 65 "[APT]  Couldn't update."
}

# Wrapper around dnf initialization (RedHat Linux and derivatives)
#
# Args:
#     NULL
# Returns:
#     NULL
package_dnf_initialize () {
    # shellcheck disable=SC2317
    sudo dnf -y update || clean_exit 65 "[DNF]  Couldn't update."
}

# Dummy weight
#
# Args:
#     NULL
# Returns:
#     NULL
package_pacman_initialize () {
    # shellcheck disable=SC2317
    true
}

# Wrapper around zypper initialization (Suse Linux and derivatives)
#
# Args:
#     NULL
# Returns:
#     NULL
package_zypper_initialize () {
    # shellcheck disable=SC2317
    sudo zypper ref || clean_exit 65 "[ZYPR]  Couldn't update."
}

# Wrapper around apk installation (Alpine Linux and derivatives)
#
# Args:
#     $@: items to install
# Returns:
#     NULL
package_apk_install () {
    # shellcheck disable=SC2317
    apk add "$@" || clean_exit 65 "[APK]  Couldn't install $*."
    # sudo apk update || clean_exit 65 "[APK]  Couldn't update."
}

# Wrapper around apt installation (Debian Linux and derivatives)
#
# Args:
#     $@: items to install
# Returns:
#     NULL
package_apt_install () {
    # shellcheck disable=SC2317
    sudo apt install -y "$@" || clean_exit 65 "[APT]  Couldn't install $*."
}

# Wrapper around dnf installation (RedHat Linux and derivatives)
#
# Args:
#     $@: items to install
# Returns:
#     NULL
package_dnf_install () {
    # shellcheck disable=SC2317
    sudo dnf -y install "$@" || clean_exit 65 "[DNF]  Couldn't install $*."
}

# Wrapper around pacman installation (Arch Linux and derivatives)
#
# Args:
#     $@: items to install
# Returns:
#     NULL
package_pacman_install () {
    # shellcheck disable=SC2317
    sudo pacman --noconfirm -Syu "$@" \
        || clean_exit 65 "[ARCH]  Couldn't install $*."
}

# Wrapper around zypper installation (Suse Linux and derivatives)
#
# Args:
#     $@: items to install
# Returns:
#     NULL
package_zypper_install () {
    # shellcheck disable=SC2317
    sudo zypper -n install "$@" || clean_exit 65 "[ZYPR]  Couldn't install $*."
}

###############################################################################

#######################
#                     #
#   MacOS FUNCTIONS   #
#                     #
#######################

# Install fonts using brew cask
#
# Args:
#     NULL
# Returns:
#     NULL
macos_install_fonts () {
    printf "[INFO] installing fonts using homebrew.\n"
    mac_fonts=""
    for entry in ${fonts}; do
        font_name="$(echo "${entry}" | cut -d= -f1)"
        if [ -z "${mac_fonts}" ]; then
            mac_fonts="font-${font_name}"
        else
            mac_fonts="${mac_fonts} font-${font_name}"
        fi
    done

    # shellcheck disable=SC2086
    brew install --cask $mac_fonts
    unset mac_fonts
}

# Initialize package manager for MacOS: homebrew
# install homebrew, the missing package manager
# as prescribed here: https://brew.sh/
#
# Args:
#     NULL
# Returns:
#     NULL
# shellcheck disable=SC2317
package_homebrew_initialize () {
    printf "\n\n"
    printf "[PART] Homebrew\n"
    if command -v "brew"; then
        printf "[INFO] found that 'brew' is already installed.\n"
    else
        printf "[INFO] installing Homebrew, the missing package manager.\n"
        printf "[ACT]  You may be asked questions by 'Homebrew'.\n"
        /bin/bash -c "$(curl -fsSL \
https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    brew tap homebrew/cask-fonts
}

# Installation wrapper around default package manager
#
# Args:
#     NULL
# Returns:
#     NULL
# shellcheck disable=SC2317
package_homebrew_install () {
    deps="$*"
    if [ "${deps}" = "npm" ]; then
        # npm is the only dependency - argument passed
        deps="node"
    elif [ "${deps}" = "${deps#*npm}" ]; then
        # npm is not in deps
        true
    elif [ "${deps}" = "${deps#npm}" ]; then
        # npm is not the first dep
        deps="${deps%% npm*} node${deps#* npm}"
    else
        # npm is the first dep
        deps="node ${deps#npm }"
    fi
    # shellcheck disable=SC2086
    brew install $deps

    # What about --cask?
    unset deps
}

###############################################################################

#########################
#                       #
#    SCRIPT SEGMENTS    #
#                       #
#########################

# Initialize package manager for the platform (distribution/MacOS)
#
# Args:
#     NULL
# Returns:
#     NULL
init_package_manager () {
    printf "\n\n"
    printf "[PART] guessing and initializing the package manager.\n"
    if [ -n "${manager}" ] ; then
        package_"${manager}"_initialize
        package_install () {
            package_"${manager}"_install "$@"
        }
        return
    else
        for manager in $package_managers; do
            if command -v "${manager}" >/dev/null 2>&1; then
                printf "[INFO] initializing package manager '%s'\n." \
                       "${manager}"
                package_"${manager}"_initialize
                package_install () {
                    package_"${manager}"_install "$@"
                }
                return
            fi
        done
    fi
    clean_exit 1 "Supported package managers: ${package_managers}."
}

# dependencies
#
# Args:
#     NULL
# Returns:
#     NULL
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

# Back up existing Emacs from standard locations with a .bak extension
#
# Args:
#     NULL
# Returns:
#     NULL
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

# reset backups to original
#
# Args:
#     NULL
# Returns:
#     NULL
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

# Location for $LOCAL_EMACS_HOME
#
# Args:
#     NULL
# Returns:
#     NULL
set_local_home () {
    printf "[INFO] Setting a local cache for Emacs packages.\n"
    if [ -n "${LOCAL_EMACS_HOME}" ]; then
        printf "[INFO] LOCAL_EMACS_HOME is set to %s, using it.\n" \
               "${LOCAL_EMACS_HOME}"
        return
    fi

    LOCAL_EMACS_HOME="${emacs_cache}/local.d"
    printf "Do you want to set local cache in %s? [y/N]\t" "${LOCAL_EMACS_HOME}"
    read -r yn

    case "${yn}" in
        [Yy]*)
            rc_export="

# ADDED BY PSPMACS
LOCAL_EMACS_HOME=\"${LOCAL_EMACS_HOME}\"
export LOCAL_EMACS_HOME"
            if [ -n "${RUNCOMDIR}" ]; then
                printf "%s\n" "${rc_export}" \
                       >> "${XDG_CONFIG_HOME:-${HOME}/.config}/local.d/.emacsrc"
            else
                printf "%s\n" "${rc_export}" >> "${HOME}/.bashrc"
                printf "%s\n" "${rc_export}" >> "${HOME}/.zshrc"
                printf "%s\n" "${rc_export}" >> "${HOME}/.profile"
            fi
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

# Clone pspmacs installation
#
# Args:
#     NULL
# Returns:
#     NULL
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

# softlink ~/.config/emacs and ~/.emacs.d to ~/.local/share/emacs/pspmacs
#
# Args:
#     NULL
# Returns:
#     NULL
set_emacs_config () {
    printf "[PART] infecting Emacs with pspmacs config.\n"
    mkdir -p "${emacs_config}"
    ln -sf "${emacs_config}" "${HOME}/.emacs.d" || revert_backup
    ln -sf "${emacs_data}/pspmacs" "${emacs_config}" || revert_backup
}

# Print some exit information and instructions
#
# Args:
#     NULL
# Returns:
#     NULL
printf_byelogue () {
    bye_message="



    [INFO] Unless you see errors above, Emacs is installed and PSPMacs cloned.

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
#
# Args:
#     Command line arguments, usually "$@"
# Returns:
#     NULL
main () {
    set_vars
    cli "$@"
    printf "[INFO] We shall attempt an automated installation for %s.\n"\
           "${ostype}"
    printf "[INFO] loading functions.\n"
    init_package_manager || clean_exit 65
    if $include_deps; then
        install_dependencies || clean_exit 65
    fi
    if $include_fonts; then
        printf "[PART] Fonts\n"
        case "${ostype}" in
            [Ll]inux)
                linux_install_fonts || clean_exit 65
                ;;
            [Dd]arwin)
                macos_install_fonts || clean_exit 65
                ;;
            *)
                clean_exit \
                    66 "[FAIL] Currently supported: GNU/Linux and Darwin.\n"
            ;;
        esac
    fi
    clone_pspmacs || clean_exit 65
    backup_std_emacs
    set_local_home
    set_emacs_config
    unset_vars
    printf_byelogue
    clean_exit
}

main "$@"
