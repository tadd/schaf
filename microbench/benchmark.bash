#!/bin/bash -eu
set -o pipefail

nloop=4

run() {
    local -r bin="$1"
    echo "# $bin"
    for t in *.scm; do
        echo $t
	benchmark-run -n $nloop bash -c "time $bin $t" 2>&1
    done | ruby stat.rb
}

main() {
    local bin=schaf
    if (( $# >= 1 )); then
        bin="$1"
    fi
    run "$bin"
}

main "$@"
