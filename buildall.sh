# builds "hard" prerequisites and llpp
set -e

filt='^$'
btyp="release"
mupdfrev=745cc918409d3a5bd45b7b5c025f3a7a8c47c633
while getopts j:f:t:r: opt; do
    case "$opt" in
        j) jobs="-j $OPTARG";;
        f) filt="$OPTARG";;
        t) btyp="$OPTARG";;
        r) mupdfrev="$OPTARG";;
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

lablgl=http://wwwfun.kurims.kyoto-u.ac.jp/soft/lsl/dist/lablgl-1.05.tar.gz
baseurl="http://git.ghostscript.com/"

mudir=mupdf-$mupdfrev
mutgz=mupdf-$mupdfrev.tgz
muurl="${baseurl}?p=mupdf.git;a=snapshot;h=$mupdfrev;sf=tgz"

test -d lablgl-1.05 || (wget -nc $lablgl && tar -xzf lablgl-1.05.tar.gz)
test -e $mutgz || wget -nc $muurl -O $mutgz
mkdir $mudir && tar --strip-components 1 -C $mudir -xzf $mutgz

fetch() {
    while read r m; do
        t=$m-$r.tgz
        test $m = jbig2dec && p=$m || p=thirdparty/$m
        u="${baseurl}?p=$p.git;a=snapshot;h=$r;sf=tgz"
        test -e $t || wget -nc $u -O $t
        test -e $mudir/thirdparty/$m/README ||
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

executable_p() {
    command -v $1 >/dev/null 2>&1
}

executable_p gmake && make=gmake || make=make

(cd lablgl-1.05                                            \
    && sed 17d Makefile.config.linux.mdk > Makefile.config \
    && $make lib $(test "$1" = opt && echo libopt)         \
    && $make install                                       \
            BINDIR=$root/bin                               \
            LIBDIR=$root/lib/ocaml                         \
            DLLDIR=$root/lib/ocaml/stublibs                \
            INSTALLDIR=$root/lib/ocaml/lablGL)

(cd $mudir && $make $jobs build=$btyp)

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

ccopt="$ccopt -D_GNU_SOURCE"

cclib="$cclib -L$root/$mudir/build/$btyp"
cclib="$cclib -lmupdf"
cclib="$cclib -lz -ljpeg -lopenjpeg -ljbig2dec -lpthread -lcrypto"
cclib="$cclib -lX11"

expr "$filt" : '.*freetype.*' && {
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
    cmsuf=cmx
    dolink() {
        $comp -o llpp                      \
            -I $root/lib/ocaml/lablGL      \
            str.cmxa unix.cmxa lablgl.cmxa \
            link.o                         \
            -cclib "$cclib"                \
            help.cmx                       \
            utils.cmx                      \
            parser.cmx                     \
            wsi.cmx                        \
            main.cmx
    }
else
    executable_p ocamlc.opt && comp=ocamlc.opt || comp=ocamlc
    cmsuf=cmo
    dolink() {
        $comp -custom -o llpp           \
            -I $root/lib/ocaml/lablGL   \
            str.cma unix.cma lablgl.cma \
            link.o                      \
            -cclib "$cclib"             \
            help.cmo                    \
            utils.cmo                   \
            parser.cmo                  \
            wsi.cmo                     \
            main.cmo
    }
fi

$comp -c -o link.o -ccopt "$ccopt" $srcpath/link.c
$comp -c -o help.$cmsuf help.ml
$comp -c -o utils.$cmsuf $srcpath/utils.ml
$comp -c -o wsi.cmi $srcpath/wsi.mli
$comp -c -o wsi.$cmsuf $srcpath/wsi.ml
$comp -c -o parser.$cmsuf $srcpath/parser.ml
$comp -c -o main.$cmsuf -I $root/lib/ocaml/lablGL $srcpath/main.ml
dolink

echo All done
