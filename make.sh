#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_lazbuild
(
    if ! (which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
                ;;
        esac
    fi
    if [[ -f 'use/components.txt' ]]; then
        git submodule update --init --recursive
        git submodule update --recursive --remote
        while read -r; do
            if [[ -n "${REPLY}" ]] &&
                ! (lazbuild --verbose-pkgsearch "${REPLY}") &&
                ! (lazbuild --add-package "${REPLY}") &&
                ! [[ -e "use/${REPLY}" ]]; then
                    declare -A VAR=(
                        [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                        [out]=$(mktemp)
                    )
                    wget --output-document "${VAR[out]}" "${VAR[url]}" 2>/dev/null
                    unzip -o "${VAR[out]}" -d "use/${REPLY}"
                    rm --verbose "${VAR[out]}"
                fi
        done < 'use/components.txt'
        find 'use' -type 'f' -name '*.lpk' -exec lazbuild --add-package-link {} +
    fi
    find 'src' -type 'f' -name '*.lpi' \
        -exec lazbuild --no-write-project --recursive --no-write-project --build-mode=release {} + 1>&2
)

function priv_main
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) priv_lazbuild ;;
            *) priv_clippit ;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}" >/dev/null
