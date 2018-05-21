#!/bin/bash
set -eu

now() { date +%s; }
tstart=$(now)
vecho() { ${vecho-:} "$*"; }
digest() { cksum 2>/dev/null $* | while read h _; do printf $h; done; }

partmsg() {
    test $? -eq 0 && msg="ok" || msg="ko"
    echo "$msg $(($(now)-tstart)) sec"
}

die() {
    echo "$*" >&2
    exit 111
}

trap 'partmsg' EXIT

darwin=false
wsi="wsi/x11"
case "$(uname)" in
    Darwin)
        darwin=true
        wsi="wsi/osx"
        mjobs=$(getconf _NPROCESSORS_ONLN || echo 1)
        ;;
    Linux) mjobs=$(getconf _NPROCESSORS_ONLN || echo 1);;
    OpenBSD) mjobs=$(getconf NPROCESSORS_ONLN || echo 1);;
    *) die $(uname) is not supported;;
esac

test -n "${1-}" || die "usage: $0 build-directory"

outd="$1"
srcd="$(dirname $0)"
mudir=$outd/mupdf
muinc="-I $mudir/include -I $mudir/thirdparty/freetype/include"

mkdir -p $outd/$wsi
mkdir -p $outd/lablGL
:>$outd/ordered

isfresh() { . 2>/dev/null "$1.past" && test "$k" = "$2"; }

mulibs="$mudir/build/native/libmupdf.a" # $mudir/build/native/libmupdf-third.a
keycmd="(cd $mudir && git describe --tags --dirty); digest $mulibs"
isfresh "$mulibs" "$(eval $keycmd)" || (
    make -C "$mudir" build=native -j $mjobs libs
    echo "k=\"$(eval $keycmd)\"" >$mudir/build/native/libmupdf.a.past
) && vecho "fresh mupdf"

oflags() {
    case "${1#$outd/}" in
        config.cmo                              \
        | glutils.cmo                           \
        | help.cmo                              \
        | keys.cmo                              \
        | listview.cmo                          \
        | main.cmo                              \
        | parser.cmo                            \
        | unisyms.cmo                           \
        | utils.cmo                             \
        | $wsi/wsi.cmo)         # XXX confstruct?
            f="-g -strict-sequence -strict-formats -warn-error @A";;
        *) f="-g";;
    esac
    echo "$incs $f"
}

cflags() {
    f="-g -std=c99 -O2 $muinc -Wall -Werror -Wextra -pedantic-errors"
    case "${1#$outd/}" in
        cutils.o) echo "$f";;
        version.o) echo '-DLLPP_VERSION="'$ver'"';;
        link.o) $darwin && echo "$f -D__COCOA__" || echo "$f";;
        */keysym2ucs.o) echo "-O2 -include inttypes.h -DKeySym=uint32_t";;
        */ml_*.o) echo "-g -Wno-pointer-sign -O2";;
        *) echo "-g -O2";;
    esac
}

mflags() { echo "-I $(ocamlc -where) -g -O2"; }

incs="-I $srcd/lablGL -I $srcd/$wsi -I $srcd"
incs="$incs -I $outd/lablGL -I $outd/$wsi -I $outd"

overs="$(ocamlc --version 2>/dev/null)" || overs="0.0.0"
oversnum="$(echo $overs | { IFS=. read a b _; echo $a$b; })"

test $oversnum -ge 407 || {
    url=https://github.com/ocaml/ocaml/archive/4.07.zip
    zip=$outd/$(basename $url)
    isfresh $zip $url || {
        executable_p() { command -v "$1" >/dev/null 2>&1; }
        if executable_p wget; then dl() { wget -q "$1" -O "$2"; }
        elif executable_p curl; then dl() { curl -L "$1" -o "$2"; }
        else die "no program to fetch remote urls found"
        fi
        dl $url $zip
        echo "k=$url" >$zip.past
    } && vecho "fresh $zip"
    absprefix=$(cd $outd &>/dev/null; pwd -P)
    export PATH=$absprefix/bin:$PATH
    isfresh $absprefix/bin/ocamlc "$url" || (
        unzip -o -u -d $outd $zip
        bn=$(basename $url)
        cd $outd/ocaml-${bn%.zip}
        ./configure -prefix $absprefix                                      \
                    -no-graph -no-debugger -no-ocamldoc -no-native-compiler
        make -j $mjobs world
        make install
        echo "k='$url'" >$absprefix/bin/ocamlc.past
    ) && vecho "fresh ocamlc"
    overs=$(ocamlc --version 2>/dev/null) || overs="0.0.0"
    oversnum=$(echo $overs | { IFS=. read a b _; echo $a$b; })
}

bocaml1() {
    local n=$1
    local s="$2"
    local o="$3"
    local O=${4-}
    local dd

    local cmd="ocamlc -depend -bytecode -one-line $incs $s"
    local keycmd="digest $o $s"
    isfresh "$o.depl" "$overs$cmd$(eval $keycmd)" || {
        eval "$cmd" | {
            read _ _ depl
            :>"$o.depl"
            for d in $depl; do
                local D=${d#$srcd/}
                test "$O" = "$D" || {
                    bocaml "$D" $((n+1))
                    case $d in
                        $outd/*) dd=$d;;
                        *) dd=$outd/${d#$srcd/};;
                    esac
                    printf "$dd " >>"$o.depl"
                }
            done
        } || die "$cmd failed"
        echo "k='$overs$cmd$(eval $keycmd)'" >"$o.depl.past"
    } && {
        vecho "fresh $o.depl"
        for d in $(< $o.depl); do
            bocaml ${d#$outd/} $((n+1))
        done
    }

    cmd="ocamlc $(oflags $o) -c -o $o $s"
    keycmd="digest $o $s $(< $o.depl)"
    grep -q "$o" $outd/ordered || {
        echo "$o" >>"$outd/ordered"
        isfresh "$o" "$overs$cmd$(eval $keycmd)" || {
            printf "%*.s%s -> %s\n" $n '' "${s#$srcd/}" "${o#$outd/}"
            eval "$cmd || die '$cmd failed'"
            echo "k='$overs$cmd$(eval $keycmd)'" >"$o.past"
        } && vecho "fresh '$o'"
    }
}

bocaml() (
    local o="$1"
    local n="$2"
    local wocmi="${o%.cmi}"
    local s
    case ${wocmi#$outd/} in
        confstruct.cmo)
            s=$outd/confstruct.ml
            o=$outd/confstruct.cmo
            ;;
        *)
            test "$o" = "$wocmi" && s=$srcd/${o%.cmo}.ml || s=$srcd/$wocmi.mli
            o=$outd/$o
            ;;
    esac
    bocaml1 $n "$s" "$o"
    case $wocmi in
        wsi) s="$srcd/$wsi/wsi.ml";;
        help) s="$srcd/help.ml";;
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
        eval "$cmd || die '$cmd failed'"
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
        eval "$cmd || die '$cmd failed'"
        read _ d <$o.dep
        echo "k='$cmd$(eval $keycmd)'" >"$o.past"
    } && vecho "fresh $o"
}

ver=$(cd $srcd && git describe --tags --dirty) || ver=unknown

cmd="zsh $srcd/genconfstr.sh >$outd/confstruct.ml"
keycmd="digest $srcd/genconfstr.sh $outd/confstruct.ml"
isfresh "$outd/confstruct.ml" "$cmd$(eval $keycmd)" || {
    echo genconfstr
    eval "$cmd || die genconfstr.sh failed"
    echo "k='$cmd$(eval $keycmd)'" > "$outd/confstruct.ml.past"
} && vecho "fresh $outd/confstruct.ml"

shift 1
for target; do
    case "$target" in
        doc)
            doct=${doct-manpage}
            md=$outd/doc
            mkdir -p $md
            case $doct in
                epub) suf=.epub;;
                manpage) suf=.1;;
                *) die "unknown doc type";;
            esac
            for m in llpp llppac llpphtml; do
                src=$srcd/adoc/$m.adoc
                out=$md/$m$suf
                conf="$srcd/man/asciidoc.conf"
                keycmd="digest $out $src $conf"
                cmd="a2x -D $md -d manpage -f $doct $src"
                isfresh "$out" "$cmd$(eval $keycmd)" || {
                    echo "$src -> $out"
                    eval "$cmd || die '$cmd failed'"
                    echo "k='$cmd$(eval $keycmd)'" >"$out.past"
                } && vecho "fresh manual pages"
            done;;

        completions) die "not yet";;

        *) die "no clue - '$target'";;
    esac
done

bocaml main.cmo 0

cobjs=
for m in link cutils version; do
    bocamlc $m.o
    cobjs="$cobjs $outd/$m.o"
done

libs="str.cma unix.cma"
clibs="-L$mudir/build/native -lmupdf -lmupdf-third -lpthread"
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
cmd=$(echo $cmd $globjs -cclib \"$clibs\")
keycmd="digest $outd/llpp $cobjs $ord $mulibs"
isfresh "$outd/llpp" "$cmd$(eval $keycmd)" || {
    echo linking $outd/llpp
    eval "$cmd || die '$cmd failed'"
    echo "k='$cmd$(eval $keycmd)'" >"$outd/llpp.past"
} && vecho "fresh llpp"

if $darwin; then
    out="$outd/llpp.app/Contents/Info.plist"
    keycmd="digest $out $srcd/wsi/osx/genplist.sh"
    isfresh $out "$(eval $keycmd)" || {
        shortver=$(echo $ver | { IFS='-' read s _; echo ${s#v}; })
        d=$(dirname $out)
        mkdir -p "$d"
        . $srcd/wsi/osx/genplist.sh >"$out"
        echo "k='$(eval $keycmd)'" >"$out.past"
    } && vecho "fresh plist"

    out=$outd/llpp.app/Contents/MacOS/llpp
    keycmd="digest $out $outd/llpp"
    isfresh $out "$(eval $keycmd)" || {
        mkdir -p "$(dirname $out)"
        cp $outd/llpp $out
        echo "k='$(eval $keycmd)'" >"$out.past"
    } && vecho "fresh bundle"
fi
