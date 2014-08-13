#!/bin/sh
set -e

compiler="$1"
out="$2"
in="$3"
outdir="$(dirname "$out")"
srcdir="$(pwd -P)"
shift 3

dodep ()
{
    (cd >/dev/null $srcdir && ocamldep $(basename $in)) |       \
        (sed >$objdir/$out.d                                    \
             -e "/.cmx/d"                                       \
             -e 's;\([[:alnum:]\.]\+\);'$outdir'/\1;g'          \
             -e '/:$/d')
}

expr >/dev/null "$*" : '.* -pp ' && {
    ef=$(mktemp)
    trap 'test -n "$ef" && rm -f "$ef"' 0
    (cd >/dev/null $outdir && $compiler 2>$ef "$@" -o $out $in)
    rc=$?
    if test "$rc" != 0; then
        sed 1>&2 "s;File \"\([^\"]*\)\"\(.*\)$;File $in\2;" $ef
    else
        dodep
    fi
    exit $rc
} || {
    (cd >/dev/null $outdir && $compiler "$@" -o $out $in) && dodep
}
