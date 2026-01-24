#!/bin/bash -u
readonly scriptdir=$(realpath "$0" | xargs dirname)
readonly srcdir=$(realpath --relative-to=$PWD "$scriptdir/..")
readonly testdir=$(realpath --relative-to=$PWD "$scriptdir/error")
readonly schaf=$srcdir/${SCHAF:-schaf}

verbose=0
if [[ "${1-x}" = -v ]]; then
    verbose=1
    echo using $schaf >&2
fi

for f in "$testdir"/*.scm; do
    msg=`head -1 "$f" | sed 's/^;* *//; s/ *$//'`
    if [ -z "$msg" ]; then
        tail +2 "$f" | $schaf -S -
    else
        tail +2 "$f" | $schaf -S - 2>&1 | grep -q "$msg"
    fi
    ret=$?
    if [ $ret -ne 0 ]; then
        echo failure: "$f" >&2
        exit $ret
    elif (( verbose )); then
        echo success: "$f" >&2
    fi
done

echo error test succeeded. >&2
