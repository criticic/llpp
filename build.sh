#!/bin/sh
set -eu

date --version | grep -q "GNU" && dfmt="%s.%N" || dfmt="%s"
now() { date +$dfmt; }

tstart=$(now)
vecho=${vecho-:}
command -v md5sum >/dev/null && alias sum='md5sum 2>/dev/null'

partmsg() {
    test $? -eq 0 && msg="ok" || msg="ko"
    echo "$msg $(echo "scale=3; ($(now) - $tstart)/1" | bc -l) sec"
}

die() {
    echo "$*" >&2
    exit 111
}

trap 'partmsg' EXIT

test -n "$1" || die "usage: $0 build-directory"

outd=$1
srcd=$PWD
mudir=$srcd/mupdf
muinc="-I $mudir/include -I $mudir/thirdparty/freetype/include"

isfresh() {
    test -e "$1" && test -r "$1.past" && {
            . "$1.past"
            test "$k" = "$2"
        }
}

_i="-I lablGL -I $outd/lablGL -I wsi/x11 -I $outd/wsi/x11 -I $outd"
oflags() {
    case "${1#$outd/}" in
        main.cmo|utils.cmo|config.cmo|parser.cmo|wsi.cmi|wsi/x11/wsi.cmo)
            echo "-g -strict-sequence -warn-error a $_i";;
        *) echo "-g $_i";;
    esac
}

cflags() {
    case "${1#$outd/}" in
        link.o) echo "-g -O2 $muinc -Wall -Werror -Wextra -pedantic-errors";;
        *) :;;
    esac
}

bocaml1() {
    eval ocamlc -depend -bytecode -one-line $incs $s | {
        read _ _ depl
        for d in $(eval echo $depl); do
            bocaml ${d#$srcd/} $((n+1))
        done
    }
    cmd="ocamlc $(oflags $o) -c -o $o $s"
    keycmd="sum $o $s"
    grep -q "$o" $outd/ordered || {
        echo "$o" >>$outd/ordered
        isfresh "$o" "$cmd$(eval $keycmd)" || {
            printf "%*.s%s -> %s\n" $n '' "${s#$srcd/}" "$o"
            eval "$cmd"
            echo "k='$cmd$(eval $keycmd)'" >$o.past
        } && $vecho "fresh '$o'"
    }
}

bocaml() (
    o="$1"
    n="$2"
    wocmi="${o%.cmi}"
    test ${wocmi%help.cmo} !=  $wocmi && {
        s=$outd/help.ml
        o=$outd/help.cmo
    } || {
        test "$o" = "$wocmi" && s=$srcd/${o%.cmo}.ml || s=$srcd/$wocmi.mli
        o=$outd/$o
    }
    incs="-I lablGL -I $outd/lablGL -I wsi/x11 -I $outd/wsi/x11 -I $outd"
    bocaml1 "$s" "$o"
)

bocamlc() {
    o=$outd/$1
    s=$srcd/${1%.o}.c
    cmd="ocamlc -ccopt \"$(cflags $o) -MMD -MF $o.dep -MT_ -o $o\" $s"
    test -r $o.dep && read _ d <$o.dep || d=
    keycmd='sum $o $d'
    isfresh "$o" "$cmd$(eval $keycmd)" || {
        printf "%s -> %s\n" "${s#$srcd/}" "$o"
        eval "$cmd"
        read _ d <$o.dep
        echo "k='$cmd$(eval $keycmd)'" >$o.past
    }
}

mkdir -p $outd/wsi/x11
mkdir -p $outd/lablGL
:>$outd/ordered

mkhelp() {
    ocaml str.cma -stdin $srcd/KEYS <<EOF
let fixup = let open Str in
  let dash = regexp {|\([^ ]*\) +- +\(.*\)|}
  and head = regexp {|-----\(.*\)-----|} in fun s ->
  String.escaped s |> global_replace dash {|\1\t\2|}
                   |> global_replace head {|\xc2\xb7\1|};;
let rec iter ic = match input_line ic with
| s -> Printf.printf "\"%s\";\\n" @@ fixup s; iter ic
| exception End_of_file -> ();;
Printf.printf "let keys = [\\n";
iter @@ open_in Sys.argv.(1);;
Printf.printf "] and version = \"$ver\";;"
EOF
}

ver=$(cd $srcd && git describe --tags) || echo unknown
cmd="mkhelp >$outd/help.ml"
keycmd="sum $srcd/KEYS; echo $ver"
isfresh "$outd/help.ml" '$cmd$(eval keycmd)$ver' || {
    eval $cmd
    echo "k='$cmd$(eval $keycmd)$ver'" >$outd/help.ml.past
}

for m in lablGL/glMisc.cmo lablGL/glTex.cmo wsi/x11/wsi.cmo main.cmo; do
    bocaml $m 0
done
bocamlc link.o

libs="str.cma unix.cma"
clibs="-lGL -lX11 -L$mudir/build/native -lmupdf -lmupdfthird -lpthread"
globjs=
for f in ml_gl ml_glarray ml_raw; do
    bocamlc lablGL/$f.o
    globjs="$globjs $outd/lablGL/$f.o"
done

ord=$(echo $(eval grep -v \.cmi $outd/ordered))
cmd="ocamlc -custom $libs -o $outd/llpp $ord"
cmd="$cmd $globjs $outd/link.o -cclib \"$clibs\""
keycmd="sum $outd/llpp $ord"
isfresh "$outd/llpp" "$cmd$(eval $keycmd)" || {
        echo linking $outd/llpp
        eval $cmd
        echo "k='$cmd$(eval $keycmd)'" >$outd/llpp.past
    }
