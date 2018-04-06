#!/bin/sh
set -e

trap 'test $? -eq 0 || echo build failed' EXIT

date --help | grep -q GNU-date-that-actually-works-with-N && {
    now() { date +%N; }
    scl=1000000000.0
} || {
    now() { date +%s; }
    scl=1
}
tstart=$(now)

die() {
    echo "$*" >&2
    exit 111
}

test -n "$1" || die "usage: $0 build-directory"

outd=$1
srcd=$PWD

getpast() {
    past="$1.past"
    cur_cmd="$2"
    key_cmd="$3"
    test -r $past && {
        . $past
        test "$cmd" = "$cur_cmd" && {
            eval "cur_key=\$($key_cmd)" || dirty=nokey && {
                    test "$cur_key" = "$key" || dirty="key"
                }
        } || dirty="cmd"
    } || dirty="initial"
}

bocaml1() {
    s="$1"
    o="$2"
    eval ocamlc -depend -bytecode -one-line $incs $s | {
        read _ _ depl
        test -z "$depl" || {
            for d in $(eval echo $depl); do
                d=${d#$srcd/}
                bocaml $d $((n+1)) || dirty=transitive
            done
        }
    }
    cmd="ocamlc $incs -c -o $o $s"
    keycmd="stat -c %Y $o $s 2>/dev/null | tr -d '\n'"
    getpast "$o" "$cmd" "$keycmd"
    test -n "$dirty" && {
        printf "%*.s%s -> %s\n" $n '' \
               "${s#$srcd/}" "${o#$outd/} [${dirty-fresh}]"
        eval "$cmd" || die "compilation failed"
        eval "key=\$($keycmd)"
        printf "cmd='$cmd'\nkey='$key'\n" >$o.past
        grep -q "$o" $outd/ordered || echo "$o" >>$outd/ordered
        return 1
    } || {
        grep -q "$o" $outd/ordered || echo "$o" >>$outd/ordered
        return 0
    }
}

bocaml() (
    o=$1
    n=$2
    wocmi="${o%.cmi}"
    test ${wocmi%help.cmo} !=  ${wocmi} && {
        s=$outd/help.ml
        o=$outd/help.cmo
    } || {
        test "$o" = "$wocmi" && {
            s=${o%.cmo}.ml
        } || {
            s=$wocmi.mli
        }
        s=$srcd/$s
        o=$outd/$o
    }
    incs="-I lablGL -I $outd/lablGL -I wsi/x11 -I $outd/wsi/x11 -I $outd"
    bocaml1 "$s" "$o" || return 1 && return 0
)

bocamlc() {
    o=$outd/$1
    s=$srcd/${1%.o}.c
    mudir=$srcd/mupdf
    muinc="-I $mudir/include -I $mudir/thirdparty/freetype/include"
    cmd="ocamlc -ccopt \"-O2 $muinc -o $o\" $s"
    keycmd="stat -c %Y $o $s 2>/dev/null | tr -d '\n'"
    getpast "$o" "$cmd" "$keycmd"
    test -n "$dirty" && {
        printf "%s -> %s\n" "${s#$srcd/}" "${o#$outd/} [${dirty-fresh}]"
        eval "$cmd" || die "compilation failed"
        eval "key=\$($keycmd)" || die "$keycmd failed"
        printf "cmd='$cmd'\nkey='$key'\n" >$o.past
        return 1
    } || return 0
}

mkdir -p $outd/wsi/x11
mkdir -p $outd/lablGL
relink=0
:>$outd/ordered

mkhelp() {
    vers=$(cd $srcd && git describe --tags || echo unknown)
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
Printf.printf "] and version = \"$vers\"";;
EOF
}

cmd="mkhelp >$outd/help.ml"
keycmd="stat -c %Y $srcd/KEYS 2>/dev/null"
getpast "$outd/help.ml" "$cmd" "$keycmd"
test -n "$dirty" && { eval $cmd || die "mkhelp failed"; relink=1; }
eval "key=\$($keycmd)" || die "$keycmd: failed"
printf "cmd='$cmd'\nkey='$key'\n" >$outd/help.ml.past

for m in lablGL/glMisc.cmo lablGL/glTex.cmo wsi/x11/wsi.cmo main.cmo; do
    bocaml $m 0 || relink=1
done
bocamlc link.o || relink=1

libs="str.cma unix.cma"
clibs="-lGL -lX11 -L$mudir/build/native -lmupdf -lmupdfthird -lpthread"
globjs=
for f in ml_gl ml_glarray ml_raw; do
    bocamlc lablGL/$f.o || relink=1
    globjs="$globjs $outd/lablGL/$f.o"
done

ord=$(grep -v \.cmi $outd/ordered | tr "\n" " ")
cmd="ocamlc -custom $libs -o $outd/llpp $ord"
cmd="$cmd $globjs $outd/link.o -cclib \"$clibs\""
keycmd="stat -c %Y $outd/llpp $ord 2>/dev/null"
getpast "$outd/llpp" "$cmd" "$keycmd"
test $relink -ne 0 || test -n "$dirty" && {
        echo "linking"
        eval $cmd
        eval "key=\$($keycmd)" || die "$keycmd: failed"
        printf "cmd='$cmd'\nkey='$key'\n" >$outd/llpp.past
    } || echo "nothing to be done"

printf "took %s sec\n" $(echo "scale=3; ($(now) - $tstart) / $scl" | bc -l)
