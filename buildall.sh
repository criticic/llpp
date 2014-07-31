# builds "hard" prerequisites and llpp
set -e

executable_p() {
    command -v $1 >/dev/null 2>&1
}

filt='^$'
btyp="release"
mupdfrev=803470e161539f9013f854188e57505f4878e891
while getopts j:f:t:r:F opt; do
    case "$opt" in
        j) jobs="-j $OPTARG";;
        f) filt="$OPTARG";;
        t) btyp="$OPTARG";;
        e) useegl=1;;
        r) mupdfrev="$OPTARG";;
        F) usefontconfig=1;;
        ?)
        echo \
        "usage: $0 [-j jobs] [-f filter] [-t build type] [-r revision] [opt]"
        exit 1;;
    esac
done
shift $(($OPTIND - 1))

mkdir -p 3rdp
cd 3rdp
root=$(pwd)

if executable_p wget; then
    dl() {
        wget -nc $1 -O $2
    }
else
    if executable_p curl; then
        dl() {
            echo "fetching " $1
            curl $1 -o $2
        }
    else
        echo "no program to fetch remote urls found"
        exit 1
    fi
fi

lablgl=http://wwwfun.kurims.kyoto-u.ac.jp/soft/lsl/dist/lablgl-1.05.tar.gz
baseurl="http://git.ghostscript.com/"

mudir=mupdf-$mupdfrev
mutgz=mupdf-$mupdfrev.tgz
muurl="${baseurl}?p=mupdf.git;a=snapshot;h=$mupdfrev;sf=tgz"

test -d lablgl-1.05 || \
    (dl $lablgl $(basename $lablgl) && tar -xzf lablgl-1.05.tar.gz)
test -e $mutgz || dl $muurl $mutgz
test -d $mudir || \
    (mkdir $mudir && tar --strip-components 1 -C $mudir -xzf $mutgz)

fetch() {
    while read r m; do
        t=$m-$r.tgz
        test $m = jbig2dec && p=$m || p=thirdparty/$m
        test $m = openjpeg && e=opj_config.h.in.user || e=README
        u="${baseurl}?p=$p.git;a=snapshot;h=$r;sf=tgz"
        test -e $t || dl $u $t
        test -e $mudir/thirdparty/$m/$e ||
        (rm -fr $mudir/thirdparty/$m && mkdir $mudir/thirdparty/$m &&
            tar -xzf $t --strip-components 1 -C $mudir/thirdparty/$m)
    done
}

grep -v -E "$filt" <<-EOF | fetch
2ef0a19842ae1172bec153225328aaaeaf130a18 freetype
11b4c3203dc5a9551720760570c82408c303cd1c jbig2dec
219d59dcfd0e6ce8a3d8c5510e29237f0b5078ed jpeg
4c7d23d2cd00cee7822a61d1e8472439bd6d53c9 openjpeg
c16b1b18ddaaf090caf321af831bccac6381a381 zlib
EOF

executable_p gmake && make=gmake || make=make

(cd lablgl-1.05                                            \
    && sed 17d Makefile.config.linux.mdk > Makefile.config \
    && $make lib $(test "$1" = opt && echo libopt)         \
    && $make install                                       \
            BINDIR=$root/bin                               \
            LIBDIR=$root/lib/ocaml                         \
            DLLDIR=$root/lib/ocaml/stublibs                \
            INSTALLDIR=$root/lib/ocaml/lablGL)

opjconfig=$root/$mudir/thirdparty/openjpeg/libopenjpeg/opj_config.h
expr "$filt" : '.*openjpeg.*' >/dev/null || {
cat >>$opjconfig <<EOF
#ifdef _BIG_ENDIAN
#define OPJ_BIG_ENDIAN 1
#endif
EOF
}

(cd $mudir && $make $jobs build=$btyp $ojbe)

cd ..

srcpath=$(dirname $0)

tp=$root/$mudir/thirdparty

ccopt="-O"
ccopt="$ccopt -I $tp/jbig2dec"
ccopt="$ccopt -I $tp/jpeg"
ccopt="$ccopt -I $tp/freetype/include"
ccopt="$ccopt -I $tp/openjpeg/libopenjpeg"
ccopt="$ccopt -I $tp/zlib"
ccopt="$ccopt -I $root/$mudir/include"

cclib="$cclib -L$root/$mudir/build/$btyp"
cclib="$cclib -lmupdf"
cclib="$cclib -lz -ljpeg -lopenjpeg -ljbig2dec -lpthread -lcrypto"
cclib="$cclib -lX11"

ccopt="$ccopt -D_GNU_SOURCE"
test -n "$usefontconfig" && {
    ccopt="$ccopt -DUSE_FONTCONFIG $(pkg-config --cflags fontconfig)"
    cclib="$cclib $(pkg-config --libs fontconfig)"
}

test -n "$useegl" && {
    ccopt="$ccopt -DUSE_EGL $(pkg-config --cflags egl)"
    cclib="$cclib $(pkg-config --libs egl)"
}

expr "$filt" : '.*freetype.*' >/dev/null && {
    ccopt="$ccopt $(freetype-config --cflags) -include ft2build.h"
    cclib="$cclib $(freetype-config --libs)"
} || {
    ccopt="$ccopt -include $tp/freetype/include/ft2build.h"
    cclib="$cclib -lfreetype"
}

echo Building llpp...

sh $srcpath/mkhelp.sh $srcpath/keystoml.ml $srcpath/KEYS > help.ml

if test "$1" = "opt"; then
    executable_p ocamlopt.opt && comp=ocamlopt.opt || comp=ocamlopt
    osuf=cmx
    asuf=cmxa
    link="$comp"
else
    executable_p ocamlc.opt && comp=ocamlc.opt || comp=ocamlc
    osuf=cmo
    asuf=cma
    link="$comp -custom"
fi

$comp -c -o link.o -ccopt "$ccopt" $srcpath/link.c
$comp -c -o bo.$osuf $srcpath/le/bo.ml
$comp -c -o help.$osuf help.ml
$comp -c -o utils.$osuf $srcpath/utils.ml
$comp -c -o wsi.cmi $srcpath/wsi.mli
$comp -c -o wsi.$osuf $srcpath/wsi.ml
$comp -c -o parser.$osuf $srcpath/parser.ml
$comp -c -o config.$osuf -I $root/lib/ocaml/lablGL $srcpath/config.ml
$comp -c -pp "sed -f $srcpath/pp.sed" -o main.$osuf \
      -I $root/lib/ocaml/lablGL $srcpath/main.ml

$link -o llpp                          \
    -I $root/lib/ocaml/lablGL          \
    str.$asuf unix.$asuf lablgl.$asuf  \
    link.o                             \
    -cclib "$cclib"                    \
    bo.$osuf                           \
    help.$osuf                         \
    utils.$osuf                        \
    parser.$osuf                       \
    wsi.$osuf                          \
    config.$osuf                       \
    main.$osuf

echo All done
