#!/bin/bash -eu
set -o pipefail

nloop=10
benchflags=-TM

run() {
    local -r bin="$1"
    sudo echo -n
    for t in *.scm; do
        printf '%12s\t' $t
	benchmark-run -n $nloop ../$bin $benchflags $t 2>&1 | ruby stat.rb
    done
}

main() {
    local bin=schaf
    if (( $# >= 1 )); then
        bin="$1"
    fi
    run "$bin"
}

main "$@"
