#!/bin/bash
set -eu

test "$(uname)" = Darwin && {
    darwin=true
    wsi="wsi/osx"
} || {
    darwin=false
    wsi="wsi/x11"
}

tstart=$(echo "print_float @@ Unix.gettimeofday ()" | ocaml unix.cma -stdin)
vecho() { ${vecho-:} "$*"; }
digest() { sum 2>/dev/null $* | while read d _; do printf $d; done; }

partmsg() {
    test $? -eq 0 && msg="ok" || msg="ko"
    ocaml unix.cma -stdin <<EOF
    Printf.printf "%.3f sec\n" @@ Unix.gettimeofday () -. $tstart
EOF
}

die() {
    echo "$*" >&2
    exit 111
}

trap 'partmsg' EXIT

test -n "${1-}" || die "usage: $0 build-directory"

outd="$1"
srcd="$(dirname $0)"
mudir=$srcd/mupdf
muinc="-I $mudir/include -I $mudir/thirdparty/freetype/include"

isfresh() {
    test -e "$1" && test -r "$1.past" && {
            . "$1.past"
            test "$k" = "$2"
        }
}

test "${USER-}" = "malc" && {
    keycmd="cd $mudir && git describe --tags --dirty"
    isfresh "$outd/mupdf" "$(eval $keycmd)" || (
        mkdir -p $outd
        make -C "$mudir" CC='ccache gcc' build=native -j4 libs && :>$outd/mupdf
        echo "k=$(eval $keycmd)" >$outd/mupdf.past
    ) && vecho "fresh mupdf"
}

oflags() {
    case "${1#$outd/}" in
        main.cmo|utils.cmo|config.cmo|parser.cmo|$wsi/wsi.cmo)
            f="-g -strict-sequence -strict-formats -warn-error a";;
        *) f="-g";;
    esac
    echo "$incs $f"
}

cflags() {
    case "${1#$outd/}" in
        link.o)
            f="-g -std=c99 -O2 $muinc -Wall -Werror -pedantic-errors"
            f="$f -D_GNU_SOURCE"
            $darwin && echo "$f -D__COCOA__" || echo $f;;
        */keysym2ucs.o) echo "-DKeySym=long";;
        */ml_gl.o) echo "-g -Wno-pointer-sign -O2";;
        *) echo "-g -O2";;
    esac
}

mflags() { echo "-I $(ocamlc -where) -g -O2"; }

incs="-I $srcd/lablGL -I $srcd/$wsi -I $srcd"
incs="$incs -I $outd/lablGL -I $outd/$wsi -I $outd"

test $(ocamlc -version | { IFS=. read a b _; echo $a$b; }) -lt 407 && {
    uri=https://caml.inria.fr/pub/distrib/ocaml-4.07/ocaml-4.07.0+beta2.tar.xz
    tar=$outd/$(basename $uri)
    isfresh $tar $uri || {
        executable_p() { command -v "$1" >/dev/null 2>&1; }
        if executable_p wget; then dl() { wget -q $1 -O $2; }
        elif executable_p curl; then dl() { curl $1 -o $2; }
        else die "no program to fetch remote urls found"
        fi
        dl $uri $tar
        echo "k=$uri" >$tar.past
    }
    absprefix=$(cd $outd &>/dev/null; pwd -P)
    export PATH=$absprefix/bin:$PATH
    isfresh $outd/bin/ocamlc "$(eval ocamlc -version)"  || (
        d=$(pwd)
        tar xf $tar -C $outd
        bn=$(basename $uri)
        cd $outd/${bn%.tar.xz}
        ./configure -prefix $absprefix
        make -s -j4 world
        make -s install
        echo "k='$(ocamlc -version)'" >$d/$outd/bin/ocamlc.past
    )
}

bocaml1() {
    local n=$1
    local s="$2"
    local o="$3"
    local O=${4-}
    local dd

    local cmd="ocamlc -depend -bytecode -one-line $incs $s"
    local keycmd="digest $s"
    isfresh "$o.depl" "$cmd$(eval $keycmd)" || {
        :>"$o.depl"
        eval "$cmd" | {
            read _ _ depl
            for d in $depl; do
                local D=${d#$srcd/}
                test "$O" = "$D" || {
                    bocaml "$D" $((n+1))
                    test $d = "$outd/help.cmo" && dd=$d || dd=$outd/${d#$srcd/}
                    printf "$dd " >>"$o.depl"
                }
            done
        } || die "$cmd failed"
        echo "k='$cmd$(eval $keycmd)'" >"$o.depl.past"
    } && {
        for d in $(< $o.depl); do
            test $d = "$outd/help.cmo" && dd=$d || dd=${d#$outd/}
            bocaml $dd $((n+1))
        done
    }

    cmd="ocamlc $(oflags $o) -c -o $o $s"
    keycmd="digest $s $(cat $o.depl)"
    grep -q "$o" $outd/ordered || {
        echo "$o" >>"$outd/ordered"
        isfresh "$o" "$cmd$(eval $keycmd)" || {
            printf "%*.s%s -> %s\n" $n '' "${s#$srcd/}" "${o#$outd/}"
            eval "$cmd" || die "$cmd failed"
            echo "k='$cmd$(eval $keycmd)'" >"$o.past"
        } && vecho "fresh '$o'"
    }
}

bocaml() (
    local o="$1"
    local n="$2"
    local wocmi="${o%.cmi}"
    local s
    test ${wocmi%help.cmo} !=  $wocmi && {
        s=$outd/help.ml
        o=$outd/help.cmo
    } || {
        test "$o" = "$wocmi" && s=$srcd/${o%.cmo}.ml || s=$srcd/$wocmi.mli
        o=$outd/$o
    }
    bocaml1 $n "$s" "$o"
    case $wocmi in
        wsi) s="$srcd/$wsi/wsi.ml";;
        */glMisc) s="$srcd/lablGL/glMisc.ml";;
        */glTex) s="$srcd/lablGL/glTex.ml";;
        *) false;;
    esac && {
        local s1=${s#$srcd/}
        bocaml1 $n "$s" "$outd/${s1%.ml}.cmo" "${o#$outd/}"
    } || true
)

bocamlc() {
    local o=$outd/$1
    local s=$srcd/${1%.o}.c
    local cmd="ocamlc -ccopt \"$(cflags $o) -MMD -MF $o.dep -MT_ -o $o\" $s"
    test -r $o.dep && read _ d <$o.dep || d=
    local keycmd='digest $o $d'
    isfresh "$o" "$cmd$(eval $keycmd)" || {
        printf "%s -> %s\n" "${s#$srcd/}" "${o#$outd/}"
        eval "$cmd" || die "$cmd failed"
        read _ d <$o.dep
        echo "k='$cmd$(eval $keycmd)'" >"$o.past"
    } && vecho "fresh $o"
}

bobjc() {
    local o=$outd/$1
    local s=$srcd/${1%.o}.m
    local cmd="$mcomp $(mflags $o) -MMD -MF $o.dep -MT_ -c -o $o $s"
    test -r $o.dep && read _ d <$o.dep || d=
    local keycmd='digest $o $d'
    isfresh "$o" "$cmd$(eval $keycmd)" || {
        printf "%s -> %s\n" "${s#$srcd/}" "${o#$outd/}"
        eval "$cmd"
        read _ d <$o.dep
        echo "k='$cmd$(eval $keycmd)'" >"$o.past"
    } && vecho "fresh $o"
}

mkdir -p $outd/$wsi
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

ver=$(cd $srcd && git describe --tags --dirty) || ver=unknown
cmd="mkhelp >$outd/help.ml # $ver"
keycmd="digest $srcd/KEYS # $ver"
isfresh "$outd/help.ml" "$cmd$(eval $keycmd)" || {
    eval $cmd
    echo "k='$cmd$(eval $keycmd)'" >"$outd/help.ml.past"
} && vecho "fresh $outd/help.ml"

case "${2-}" in
    man)
        md=$outd/man
        mkdir -p $md
        for m in llpp llppac llpphtml; do
            man=$srcd/man/$m.man
            xml=$md/$m.xml
            out=$md/$m.1
            keycmd="digest $xml $man"
            conf="$srcd/man/asciidoc.conf"
            cmd="asciidoc -d manpage -b docbook -f $conf -o '$xml' '$man'"
            isfresh "$xml" "$cmd$(eval $keycmd)" || {
                eval $cmd
                echo "k='$cmd$(eval $keycmd)'" >"$md/$m.past"
            } && vecho "fresh manual xmls"
            keycmd="digest $out $xml"
            cmd="xmlto man -o $md $xml"
            isfresh "$out" "$cmd$(eval $keycmd)" || {
                eval $cmd
                echo "k='$cmd$(eval $keycmd)'" >"$out.past"
            } && vecho "fresh manual pages"
        done
        shift;;
    *) ;;
esac

bocaml main.cmo 0

cobjs=$outd/link.o
bocamlc link.o

libs="str.cma unix.cma"
clibs="-L$mudir/build/native -lmupdf -lmupdfthird -lpthread"
if $darwin; then
    mcomp=$(ocamlc -config | grep bytecomp_c_co | { read _ c; echo $c; })
    clibs="$clibs -framework Cocoa -framework OpenGL"
    bobjc wsi/osx/wsicocoa.o
    cobjs="$cobjs $outd/wsi/osx/wsicocoa.o"
else
    clibs="$clibs -lGL -lX11"
    cobjs="$cobjs $outd/wsi/x11/keysym2ucs.o"
    bocamlc wsi/x11/keysym2ucs.o
fi

globjs=
for f in ml_gl ml_glarray ml_raw; do
    bocamlc lablGL/$f.o
    globjs="$globjs $outd/lablGL/$f.o"
done

ord=$(grep -v \.cmi $outd/ordered)
cmd="ocamlc -custom $libs -o $outd/llpp $cobjs $ord"
cmd="$cmd $globjs -cclib \"$clibs\""
keycmd="digest $outd/llpp $cobjs $ord"
isfresh "$outd/llpp" "$cmd$(eval $keycmd)" || {
        echo linking $outd/llpp
        eval $cmd || die "$cmd failed"
        echo "k='$cmd$(eval $keycmd)'" >"$outd/llpp.past"
    } && vecho "fresh llpp"

if $darwin; then
    out="$outd/llpp.app/Contents/Info.plist"
    keycmd="digest $out $srcd/misc/Info.plist.sh"
    isfresh $out "$(eval $keycmd)" || {
        shortver=$(echo $ver | { IFS='-' read s _; echo ${s#v}; })
        d=$(dirname $out)
        test -d "$d" || mkdir -p "$d"
        . $srcd/misc/Info.plist.sh >"$out"
        echo "k=$(eval $keycmd)" >"$out.past"
    } && vecho "fresh plist"

    out=$outd/llpp.app/Contents/MacOS/llpp
    keycmd="digest $out $outd/llpp"
    isfresh $out "$(eval $keycmd)" || {
        d=$(dirname $out)
        mkdir -p "$d"
        cp $outd/llpp $out
        echo "k=$(eval $keycmd)" >"$out.past"
    } && vecho "fresh bundle"
fi
