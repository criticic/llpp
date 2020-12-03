#!/bin/bash
set -eu

now() { date +%s; }
S=$(now)
vecho() { ${vecho-:} "$*"; }
executable_p() { command -v "$1" >/dev/null 2>&1; }
dgst='cksum $* | while read d _; do printf $d; done'
! executable_p b3sum || dgst='b3sum --no-names $*'
eval "digest() { $dgst; } 2>/dev/null"
die() { echo "$*" >&2; exit 111; }
trap 'echo "$(test $? -eq 0 || echo "fail ")$(($(now) - $S)) sec"' EXIT

darwin=false
wsid="wsi/x11"
clip="LC_CTYPE=UTF-8 xclip -i"
paste="LC_CTYPE=UTF-8 xclip -o"
uriop="echo 'Open "%s"' >&2"
print="echo 'Print "%s"' >&2"
mjobs=$(getconf _NPROCESSORS_ONLN || echo 1)
case "$(uname)" in
    Darwin)
        test $(getconf LONG_BIT) = 64 || die "need 64bit macOS"
        darwin=true
        wsid="wsi/cocoa"
        clip="LC_CTYPE=UTF-8 pbcopy"
        paste="LC_CTYPE=UTF-8 pbaste"
        uriop='open "%s"';;
    Linux) ;;
    *) die $(uname) is not supported;;
esac

test -n "${1-}" || die "usage: $0 build-directory"

outd="$1"
srcd="$(dirname $0)"
mudir=$outd/mupdf
muinc="-I $mudir/include -I $mudir/thirdparty/freetype/include"

test -d "$mudir" || die muPDF not found, consult $(dirname $0)/BUILDING

mkdir -p $outd/{$wsid,lablGL}
:>$outd/ordered

isfresh() { test -r $1.past && test "$(<$1.past)" = "$2"; }

mbt=${mbt:-release}
mulibst="$mudir/build/$mbt/libs"
mulibs="$mudir/build/$mbt/libmupdf.a $mudir/build/$mbt/libmupdf-third.a"

keycmd="make -C $mudir -q build=$mbt libs && digest $mulibs"
isfresh "$mulibst" "$(eval $keycmd)" || {
    make -C "$mudir" build=$mbt -j $mjobs libs
    eval $keycmd >${mulibst}.past
} && vecho "fresh mupdf"

oincs() {
    local i incs1= incs="-I $srcd -I $outd"
    case "${1#$outd/}" in
        lablGL/*) incs1="lablGL";;
        main.cmo) incs1="$wsid lablGL";;
        glutils.cmo|uiutils.cmo) incs1="lablGL";;
        *) ;;
    esac
    for i in $incs1; do
        incs+=" -I $srcd/$i -I $outd/$i"
    done
    echo $incs
}

oflags() {
    case "${1#$outd/}" in
        lablGL/*) f="-g";;
        *) f="-g -strict-sequence -strict-formats -alert @all -warn-error @A";;
    esac
    echo "$(oincs $1) $f"
}

cflags() {
    case "${1#$outd/}" in
        version.o) f=-DLLPP_VERSION=$ver;;
        */ml_*.o) f="-g -Wno-pointer-sign -Werror -O2";;
        link.o)
            f="-g -std=c11 $muinc -Wall -Werror -Wextra -pedantic "
            test "${mbt-}" = "debug" || f+="-O2 "
            $darwin && f+="-DCIDER -D_GNU_SOURCE -DGL_H='<OpenGL/gl.h>'" \
                    || f+="-D_POSIX_C_SOURCE -DGL_H='<GL/gl.h>'"
            f+=" -include $srcd/diag.h -DFIXME=0"
            f+=" -DTEXT_TYPE=GL_TEXTURE_RECTANGLE_ARB"
            #f+=" -DTEXT_TYPE=GL_TEXTURE_2D"
            ;;
        *) f="-g -O2 -Wall -Werror";;
    esac
    ! $darwin || f+=" -DGL_SILENCE_DEPRECATION"
    echo $f
}

mflags() {
    echo "-I $(ocamlc -where) -g -Wall -Werror -O2 -DGL_SILENCE_DEPRECATION"
}

overs="$(ocamlc -vnum 2>/dev/null)" || overs=""
test "$overs" = "4.11.1" || {
    url=https://caml.inria.fr/pub/distrib/ocaml-4.11/ocaml-4.11.1.tar.xz
    txz=$outd/$(basename $url)
    keycmd="printf $url; digest $txz;"
    isfresh $txz "$(eval $keycmd)" || {
        if executable_p wget; then dl() { wget -q "$1" -O "$2"; }
        elif executable_p curl; then dl() { curl -L "$1" -o "$2"; }
        else die "no program to fetch remote urls found"
        fi
        dl $url $txz
        eval $keycmd >$txz.past
    } && vecho "fresh $txz"
    absprefix=$(cd $outd &>/dev/null; pwd -P)
    export PATH=$absprefix/bin:$PATH
    ocamlc=$absprefix/bin/ocamlc
    keycmd="printf $url; digest $ocamlc;"
    isfresh $ocamlc "$(eval $keycmd)" || (
        tar xf $txz -C $outd
        bn=$(basename $url)
        cd $outd/${bn%.tar.xz}
        ./configure --disable-ocamldoc --disable-ocamltest      \
                    --enable-debugger=no --prefix=$absprefix
        make -j $mjobs world
        make install
        eval $keycmd >$absprefix/bin/ocamlc.past
    ) && vecho "fresh ocamlc"
    overs=$(ocamlc -vnum 2>/dev/null)
}

ccomp=${CAML_CC-$(set -- $(ocamlc -config | grep ^bytecomp_c_compiler:);
                           shift; echo $@)}
cvers=$($ccomp --version | { read v; echo $v; })

bocaml1() {
    grep -q "$3" $outd/ordered || {
        bocaml2 $*
        echo "$3" >>"$outd/ordered"
    }
}

bocaml2() {
    local n=$1 s="$2" o="$3" O=${4-}
    local d dd
    local cmd="ocamlc -depend -bytecode -one-line $(oincs $o) $s"
    local keycmd="digest $o $s $o.depl"

    isfresh "$o.depl" "$overs$cmd$(eval $keycmd)" || {
        { eval "$cmd" || die "$cmd failed"; } | {
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
        } || die "escaped $?"
        echo "$overs$cmd$(eval $keycmd)" >"$o.depl.past"
    } && {
        vecho "fresh $o.depl"
        for d in $(< $o.depl); do
            bocaml ${d#$outd/} $((n+1))
        done
    }

    cmd="ocamlc $(oflags $o) -c -o $o $s"
    keycmd="digest $o $s $(< $o.depl)"
    isfresh "$o" "$overs$cmd$(eval $keycmd)" || {
        printf "%*.s%s\n" $n '' "${o#$outd/}"
        eval "$cmd" || die "$cmd failed"
        echo "$overs$cmd$(eval $keycmd)" >"$o.past"
    } && vecho "fresh '$o'"
}

cycle=
bocaml() {
    local s o="$1" n="$2" cycle1="$cycle"
    local wocmi="${o%.cmi}"
    case ${wocmi#$outd/} in
        confstruct.cmo)
            s=$outd/confstruct.ml
            o=$outd/confstruct.cmo;;
        *)
            test "$o" = "$wocmi" && s=$srcd/${o%.cmo}.ml || s=$srcd/$wocmi.mli
            o=$outd/$o;;
    esac
    expr >/dev/null "$cycle" : ".*$o" && die cycle $o || cycle="$cycle$o"
    bocaml1 $n "$s" "$o"
    case $wocmi in
        wsi) s="$srcd/$wsid/wsi.ml";;
        help) s="$srcd/help.ml";;
        */glMisc) s="$srcd/lablGL/glMisc.ml";;
        */glTex) s="$srcd/lablGL/glTex.ml";;
        *) false;;
    esac && {
        local s1=${s#$srcd/}
        bocaml1 $n "$s" "$outd/${s1%.ml}.cmo" "${o#$outd/}"
    } || true
    cycle=$cycle1
}

bocamlc() {
    local o=$outd/$1
    local s=$srcd/${1%.o}.c
    local cc=${CAML_CC:+-cc "'$CAML_CC'" }
    local cmd="ocamlc $cc-ccopt \"$(cflags $o) -MMD -MF $o.dep -MT_ -o $o\" $s"
    test -r $o.dep && read _ d <$o.dep || d=
    local keycmd='digest $o $d'
    isfresh "$o" "$cvers$cmd$(eval $keycmd)" || {
        echo "${o#$outd/}"
        eval "$cmd" || die "$cmd failed"
        read _ d <$o.dep
        echo "$cvers$cmd$(eval $keycmd)" >"$o.past"
    } && vecho "fresh $o"
}

bobjc() {
    local o=$outd/$1
    local s=$srcd/${1%.o}.m
    local cmd="$mcomp $(mflags $o) -MD -MF $o.dep -MT_ -c -o $o $s"
    test -r $o.dep && read _ d <$o.dep || d=
    local keycmd='digest $o $d'
    isfresh "$o" "$cmd$(eval $keycmd)" || {
        echo "${o#$outd/}"
        eval "$cmd" || die "$cmd failed"
        read _ d <$o.dep
        echo "$cmd$(eval $keycmd)" >"$o.past"
    } && vecho "fresh $o"
}

ver=$(cd $srcd && git describe --tags --dirty) || ver="'built on $(date)'"

gen=$srcd/genconfstruct.sh
cmd="(export print paste clip uriop; . $gen >$outd/confstruct.ml)"
keycmd="{ echo '$print $paste $clip $uriop'; digest $gen $outd/confstruct.ml; }"
isfresh "$outd/confstruct.ml" "$cmd$(eval $keycmd)" || {
    echo "generating $outd/confstruct.ml"
    eval "$cmd" || die $gen failed
    echo "$cmd$(eval $keycmd)" > "$outd/confstruct.ml.past"
} && vecho "fresh $outd/confstruct.ml"

shift 1
for target; do
    case "$target" in
        doc)
            md=$outd/doc
            mkdir -p $md
            for m in llpp llppac llpphtml; do
                src=$srcd/adoc/$m.adoc
                o=$md/$m.1
                conf="$srcd/man/asciidoc.conf"
                keycmd="digest $o $src $conf"
                cmd="asciidoctor -b manpage -o $o $src"
                isfresh "$o" "$cmd$(eval $keycmd)" || {
                    echo "${o#$outd/}"
                    eval "$cmd" || die "$cmd failed"
                    echo "$cmd$(eval $keycmd)" >"$o.past"
                } && vecho "fresh $o"
            done;
            exit;;
        *) die "no such target - '$target'";;
    esac
done

bocaml main.cmo 0

cobjs=
for m in link cutils version; do
    bocamlc $m.o
    cobjs+=" $outd/$m.o"
done
for m in ml_gl ml_glarray ml_raw; do
    bocamlc lablGL/$m.o
    cobjs+=" $outd/lablGL/$m.o"
done

libs="str.cma unix.cma"
clibs="-L$mudir/build/$mbt -lmupdf -lmupdf-third -lpthread"
if $darwin; then
    mcomp=$ccomp
    clibs+=" -framework Cocoa -framework OpenGL"
    cobjs+=" $outd/wsi/cocoa/cocoa.o"
    bobjc wsi/cocoa/cocoa.o
else
    clibs+=" -lGL -lX11"
    cobjs+=" $outd/wsi/x11/keysym2ucs.o $outd/wsi/x11/xlib.o"
    bocamlc wsi/x11/keysym2ucs.o
    bocamlc wsi/x11/xlib.o
fi

ord=$(grep -v \.cmi $outd/ordered)
cmd="ocamlc -custom $libs -o $outd/llpp $cobjs $(echo $ord) -cclib \"$clibs\""
keycmd="digest $outd/llpp $cobjs $ord $mulibs"
isfresh "$outd/llpp" "$cmd$(eval $keycmd)" || {
    echo linking $outd/llpp
    eval "$cmd" || die "$cmd failed"
    echo "$cmd$(eval $keycmd)" >"$outd/llpp.past"
} && vecho "fresh llpp"

if $darwin; then
    out="$outd/llpp.app/Contents/Info.plist"
    keycmd="digest $out $srcd/wsi/cocoa/genplist.sh; echo $ver"
    isfresh $out "$(eval $keycmd)" || {
        d=$(dirname $out)
        mkdir -p "$d"
        echo "generating $out"
        (. $srcd/wsi/cocoa/genplist.sh) >"$out"
        eval $keycmd>"$out.past"
    } && vecho "fresh plist"

    out=$outd/llpp.app/Contents/MacOS/llpp
    keycmd="digest $out $outd/llpp"
    isfresh $out "$(eval $keycmd)" || {
        echo "bundling $out"
        mkdir -p "$(dirname $out)"
        cp $outd/llpp $out
        eval $keycmd>"$out.past"
    } && vecho "fresh bundle"
fi
