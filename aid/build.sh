#!/usr/bin/env sh
# -*- coding:utf-8; mode: shell-script; -*-
#
# Copyright 2020-2024 Pradyumna Paranjape
#
# This file is part of pspmacs.
# pspmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pspmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with pspmacs.  If not, see <https://www.gnu.org/licenses/>.
#

# set global variables for prady_sh_scripts here (build for Fc38)

if [ -d ".git" ] && [ -f "configure.ac" ] && [ -f "autogen.sh" ]; then
    # shellcheck disable=SC2034
    cmdlist_fetch_cmd="git fetch --recurse-submodules"

    # shellcheck disable=SC2034
    cmdlist_delta_cmd="echo \"Catching up...\
        \$(git rev-list --right-only --count \"HEAD..@{upstream}\") commits\""

    # shellcheck disable=SC2034
    cmdlist_pull_cmd="git pull --recurse-submodules"

    # shellcheck disable=SC2034
    cmdlist_tree_cmd="git clean -fdx"

    # shellcheck disable=SC2034
    cmdlist_autogen_cmd="./autogen.sh"

    # shellcheck disable=SC2034
    cmdlist_configure_cmd="./configure --prefix=\"\${HOME}/.local\" \
        --with-native-compilation --with-imagemagick --with-gnutls --with-gif \
        --with-tree-sitter --with-mailutils --with-harfbuzz --with-modules \
        --with-cairo --with-jpeg --with-rsvg --with-xft --with-xpm --with-png \
        --with-pgtk --with-dbus --with-tiff --with-webp --with-json"

    # shellcheck disable=SC2034
    cmdlist_clean_cmd="make -j \"\$(nproc)\" clean"

    # shellcheck disable=SC2034
    cmdlist_make_cmd="make -j \"\$(nproc)\""

    # shellcheck disable=SC2034
    cmdlist_install_cmd="make -j \"\$(nproc)\" install"

    # shellcheck disable=SC2034
    cmdlist_restore_src_cmd="git restore :/"

    # shellcheck disable=SC2034
    cmdlist_drop_cache_cmd="eln_cache=\"\${HOME}/.emacs.d/local.d/eln-cache/\"; \
        if [ -d \"\${eln_cache}\" ]; then \
        rm -rf \"\${eln_cache}\"; mkdir -p \"\${eln_cache}\"; fi"

    # shellcheck disable=SC2034
    cmdlist_deps="git autoconf make"

    # shellcheck disable=SC2034
    cmdlist_all_cmds="${all_cmds} fetch_cmd delta_cmd pull_cmd tree_cmd autogen_cmd \
        configure_cmd clean_cmd make_cmd install_cmd drop_cache_cmd \
        restore_src_cmd"

else
    printf "
$(ansi_str 'm')PRE-REQUISITE:$(ansi_dod)
    We shall assume that we are in GNU/Emacs source directory ...

$(ansi_str 'y')HINT:$(ansi_dod)
    ... which should typically be maintained at \"%s\".

$(ansi_str 'r')ERROR:$(ansi_dod)
    Current directory $(ansi_str 'c')'%s'$(ansi_dod) \
doesn't match features of GNU/Emacs source.


" "\${XDG_DATA_HOME}/emacs/src" "${PWD}"
fi
