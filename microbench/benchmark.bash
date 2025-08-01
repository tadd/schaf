#!/bin/bash -eu
set -o pipefail

nloop=10
benchflags=-TM

run() {
    local -r bin=$1
    for t in *.scm; do
        printf '%12s	' $t
	benchmark-run -n $nloop ../$bin $benchflags $t 2>&1|
            ruby -renumerable/statistics -e '
def mean(vals, label, n=0) = vals.grep(/#{label}/) { _1.split[1].to_f }.mean.round(n)
l = ARGF.readlines
cpu = mean(l, "CPU", 3)
mem = mean(l, "Vm")
puts "%.3f ms\t%6d kb" % [cpu, mem]'
    done
}

main() {
    local bin=schaf
    if (( $# >= 1 )); then
        bin=$1
    fi
    run $bin
}

main "$@"
