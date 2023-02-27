#!/usr/bin/env sh
# pull emacs branch, configure, make, make-install

# check if dependencies are met.
check_dependencies() {
    for dep in "$@"; do
        if ! command -v "${dep}" >/dev/null 2>&1; then
            clean_exit 127 "'${dep}' not found"
        fi
    done
}

# initialize variables
set_vars () {
    # CONST URL for EMACS
    REPO_URL="git://git.sv.gnu.org/emacs.git"

    # Emacs branch to follow
    branch=

    # Installation prefix "${prefix}/bin" must be in "${PATH}"
    prefix="${HOME}/.local"

    # local clone repo
    clone_dir="${XDG_DATA_HOME:-${HOME}/.local/share}/emacs/src"

    # Number of parallel jobs
    _nproc="$(nproc)"

    # Local repo is considered stale when it is behind remote by
    stale_at=10

    # skip "make check"
    skip_check=false

    # Configure with these flags
    # suggestions: --with-pgtk --with-mailutils --with-cairo
    #     --with-modules --with-gnutls=ifavailable
    config_flags=

    # Has anything changed since last checkout?
    repo_changes=false

    # proceed further despite hurdles
    assume=

    # help (usage)
    usage="
    usage:
    ${0} -h
    ${0} --help
    ${0} [*Optional Arguments] [*configuration flags]
"

    # help (details)
    help_msg="${usage}

    Clone [OR pull changes in] GNU Emacs, build it locally and install.
    If you have pspmacs already installed and configured,
        call '(emacs-repo/repo-install)' interactively.

    Optional Arguments
    -h\t\t\t\t\tprint usage message and exit
    --help\t\t\t\tprint this message and exit
    -b=BRANCH|--branch=BRANCH\t\tBranch to follow.
         [Default: <current> or master]
    -p=PREFIX|--prefix=PREFIX\t\tInstallation prefix.
        Emacs gets installed at PREFIX/bin,
        which must be exported in PATH.
        [Default: ${prefix}]
    -C=CLONE_DIR|--clone-dir=CLONE_DIR\tClone Emacs at location.
        [Default: ${clone_dir}]
    -j=JOBS|--jobs=JOBS\t\t\tNumber of allowed parallel jobs.
        [Default: ${_nproc}]
    -s=STALE|--stale-at=STALE\t\tLocal repo is considered stale when
        it is behind remote by
        [Default: ${stale_at}]
    --skip-check\t\t\tSkip the step 'make-check'.
    -y|--assume-yes\t\t\tProceed even if hurdles appear.
    -n|--assume-no\t\t\tDie at the first hurdle.

    Positional Arguments
    [FLAG ...]\t\t\t\tAll remaining arguments are flags for './configure'.
        suggestions: --with-pgtk --with-mailutils --with-cairo
                     --with-modules --with-gnutls=ifavailable

"
    # A list of locally defined variables to be cleaned later
    local_vars="REPO_URL branch prefix clone_dir config_flags repo_changes \
assume skip_check _nproc stale_at usage help_msg"
}

# For testing and debugging
# print parsed cli command and exit
_print_parsed () {
    for l_var in ${local_vars}; do
        # shellcheck disable=SC3053
        printf "%s: %s\n" "${l_var}" "${!l_var}"
    done
    clean_exit 0
}

# clean local variables
unset_vars () {
    # shellcheck disable=SC2086
    unset ${local_vars}
    unset local_vars
}

# Clean locally defined variables and exit optionally with error code and error message
#
# Args:
#    $1: Error exit code [DEFAULT = 0]
#    $2: Exit message
#
# $2 is sent to STDERR only if error code is non-zero.
clean_exit () {
    # $1: exit command
    # $2: stderr
    if [ -n "${1}" ] && [ "${1}" -ne "0" ]; then
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

# parse command-line flags
cli() {
    while test $# -ge 1; do
        case $1 in
            -h)
                clean_exit 0 "${usage}"
                ;;
            --help)
                clean_exit 0 "${help_msg}";
                ;;
            -b|--branch|-b=*|--branch=*)
                if [ ! "${1#*=}" = "${1}" ]; then
                    branch="$(echo "$1" | cut -d "=" -f 2)"
                else
                    shift
                    branch="${1}"
                fi
                shift
                ;;
            -p|--prefix|-p=*|--prefix=*)
                if [ ! "${1#*=}" = "${1}" ]; then
                    prefix="$(echo "$1" | cut -d "=" -f 2)"
                else
                    shift
                    prefix="${1}"
                fi
                prefix="$(realpath "${prefix}")"
                shift
                ;;
            -C|--clone-dir|-C=*|--clone-dir=*)
                if [ ! "${1#*=}" = "${1}" ]; then
                    clone_dir="$(echo "$1" | cut -d "=" -f 2)"
                else
                    shift
                    clone_dir="${1}"
                fi
                clone_dir="$(realpath "${clone_dir}")"
                shift
                ;;
            -j|--jobs|-j=*|--jobs=*)
                if [ ! "${1#*=}" = "${1}" ]; then
                    _nproc="$(echo "$1" | cut -d "=" -f 2)"
                else
                    shift
                    _nproc="${1}"
                fi
                shift
                ;;
            -s|--stale-at|-s=*|--stale-at=*)
                if [ ! "${1#*=}" = "${1}" ]; then
                    stale_at="$(echo "$1" | cut -d "=" -f 2)"
                else
                    shift
                    stale_at="${1}"
                fi
                shift
                ;;
            --skip-check)
                skip_check=true
                shift
                ;;
            -y|--assume-yes)
                assume=true
                shift
                ;;
            -n|--assume-no)
                assume=no
                shift
                ;;
            *)
                if [ -z "${config_flags}" ]; then
                    config_flags="${1}"
                else
                    config_flags="${config_flags} ${1}"
                fi
                shift 1
                ;;
        esac
    done
}

# Check if $prefix/bin is in $PATH
check_accessible () {
    if [ "${PATH#*"${prefix}"/bin}" = "${PATH}" ]; then
        printf "WARNING: %s is not in PATH.\n" "${prefix}/bin" >&2
    fi
}

# Confirm that despite some failure, do we still wish to continue
get_confirmation () {
    if [ -n "${assume}" ]; then
        if $assume; then
            printf "Continuing despite failure, since flag was set.\n"
        else
            clean_exit 67 "Dying on first failure, since flag was set."
        fi
    fi
    printf "The last step failed. Do you want to continue?\t"
    read -r _REPLY
    if [ "${_REPLY}" = "${_REPLY#Y}" ] && [ "${_REPLY}" = "${_REPLY#y}" ]; then
        clean_exit 67 "\nDying..."
    else
        printf "\nContinuing, this may be risky.\n"
    fi
    unset _REPLY
}

# Clone or pull REPO
get_remote () {
    if [ -d "${clone_dir}" ]; then
        git_refresh
    else
        git_clone
    fi
}

# Build Emacs
build_emacs () {
    # Remember: current working directory
    cwd="${PWD}"

    printf "\nBuilding Emacs.\n"
    printf "Entering %s ..." "${clone_dir}"
    cd "${clone_dir}" || clean_exit 1 "Can't enter ${clone_dir}."

    printf "Running autogen.sh\n"
    ./autogen.sh || clean_exit 66 "Failed..."

    printf "Configure with flags: %s to install at prefix %s.\n" \
           "${config_flags}" "${prefix}"
    # shellcheck disable=SC2086
    ./configure --prefix="${prefix}" ${config_flags} || clean_exit 66 "Failed."

    printf "make with %s parallel jobs.\n" "${_nproc}"
    make -j "${_nproc}" || clean_exit 66 "Failed..."

    if ! $skip_check; then
        printf "make check\n"
        make -j "${_nproc}" check || get_confirmation
    fi

    printf "make install\n"
    make -j "${_nproc}" install || clean_exit 66 "Failed..."

    printf "Successfully installed Emacs @ %s.\n" "${prefix}/bin"

    printf "make clean\n"
    make -j "${_nproc}" clean || true

    printf "Returning to %s.\n" "${cwd}"

    cd "${cwd}" || true
    unset cwd
}

# Check out a different branch if $branch is provided.
git_checkout_branch () {
    if [ -n "${branch}" ]; then

        printf "Checking out branch %s.\n" "${branch}"

        # List of remote branches. (Throws if git fails on ${clone_dir})
        avail_branches="$(git -C "${clone_dir}" branch -r --list | \
sed 's,origin/,,')" || clean_exit 65 "Failed: check ${clone_dir}."

        if [ "${avail_branches#*"${branch}"}" = "${avail_branches}" ]; then
            clean_exit 65 "Requested branch is not in remote."
        fi

        git -C "${clone_dir}" checkout "${branch}"

        if ! git -C "${clone_dir}" rev-parse \
             --abbrev-ref "${branch}@{u}" >/dev/null 2>&1; then

            printf "Setting upstream for %s as origin/%s.\n" \
                   "${branch}" "${branch}"

            git -C "${clone_dir}" --set-upstream-to="origin/${branch}"
        fi
    fi
    unset avail_branches
}

# Refresh local repository.
git_refresh () {
    git_checkout_branch
    printf "\nFetching remote changes.\n"
    git -C "${clone_dir}" fetch

    if [ "$(git -C "${clone_dir}" rev-list --count --right-only \
        "HEAD...@{upstream}")" -gt "${stale_at}" ]; then
        repo_changes=true
        printf "Found remote changes; rebasing on them...\n"
        git -C "${clone_dir}" rebase
    fi
    unset current_head_hash new_head_hash
}

# Clone Emacs anew
git_clone () {
    if [ -z "${branch}" ]; then
        branch="master"
    fi

    printf "Cloning Emacs '%s' branch to %s" "${branch}" "${clone_dir}"

    git clone -b "${branch}" "${REPO_URL}" "${clone_dir}" \
        || clean_exit 65 "Failed, check ${clone_dir}."

    repo_changes=true
}

# main call routine
main () {
    check_dependencies "git" "autoconf" "makeinfo"
    set_vars
    cli "$@"
    get_remote
    if ${repo_changes}; then
        build_emacs
        check_accessible
    else
        printf "Nothing new to compile.\n"
    fi
    clean_exit 0
}

main "$@"
