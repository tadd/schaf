#!/bin/bash -u
readonly scriptabsdir=$(realpath "$0" | xargs dirname)
readonly scriptdir=$(realpath --relative-to=$PWD "$scriptabsdir")
readonly srcdir=$(realpath --relative-to=$PWD "$scriptabsdir/../..")
readonly schaf=$srcdir/${SCHAF:-schaf}

cecho() {
    local -r c="$1"; shift
    printf '\033[%sm' $c
    echo -n "$@"
    printf '\033[0m\n'
}
echoexp() { cecho '1;32' "$@"; }
echoact() { cecho '1;31' "$@"; }

fail() {
    local -r f="$1" msg="$2" out="$3"
    echo Failure: "$f"
    if [ -z "$msg" ]; then
        printf "\tExpect to "
        echoexp -n be empty
        printf '\t'
    else
        printf "\tExpect to match\n"
        echoexp '"'"$msg"'"'| sed 's/^/\t\t/g'
        printf '\t'
    fi
    printf "but got\n"
    echoact '"'"$out"'"' | sed 's/^/\t\t/g'
    : $(( ++nfail ))
}

verbose=0
if [[ "${1-x}" = -v ]]; then
    verbose=1
    echo using $schaf >&2
fi

nfail=0
for f in "$scriptdir"/*.scm; do
    msg=`head -1 "$f" | sed 's/^;* *//; s/ *$//'`
    out=`tail +2 "$f" | $schaf -S - 2>&1`
    pret=$PIPESTATUS
    if [ -z "$msg" ]; then
        [ -z "$out" ] && [ $pret -eq 0 ]
    else
        echo "$out" | grep -q "$msg"
    fi
    ret=$?
    if [ $ret -ne 0 ]; then
        fail "$f" "$msg" "$out" >&2
    elif (( verbose )); then
        echo Success: "$f" >&2
    fi
done

if [ $nfail -eq 0 ]; then
    echo "Error test: Succeeded."
else
    echo "Error test: $nfail test(s) failed."
fi >&2
