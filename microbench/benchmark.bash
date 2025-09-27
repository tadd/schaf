#!/bin/bash -eu
set -o pipefail

nloop=10
benchflags=-TM

label() {
    printf 'Test\tTime (msec)\tMemory (KiB)\n'
}

run() {
    local -r bin="$1"
    sudo echo -n
    label
    for t in *.scm; do
        printf '%s\t' ${t/.scm/}
	benchmark-run -n $nloop ../$bin $benchflags $t 2>&1 | ruby stat.rb
    done
}

main() {
    local bin=schaf out=/dev/null
    if (( $# >= 1 )); then
        bin="$1"
    fi
    if (( $# >= 2 )) && [[ ! -z "$2" ]]; then
        out="$2"
    fi
    run "$bin" | tee "$out"
}

main "$@"
