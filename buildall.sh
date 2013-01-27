# builds "hard" prerequisites and llpp
set -e

jobs=1
while getopts j: opt; do
    case "$opt" in
        j) jobs="$OPTARG";;
        ?)
        printf "usage: $0 [-j N] [opt]\n";
        exit 1;;
    esac
done
shift $(($OPTIND - 1))

mkdir -p 3rdp
cd 3rdp
root=$(pwd)

lablgl=http://wwwfun.kurims.kyoto-u.ac.jp/soft/lsl/dist/lablgl-1.04.tar.gz
mupdf3p=http://mupdf.com/download/archive/mupdf-thirdparty-2012-08-14.zip
mupdfrev=a242b4cf1123910c4dba18c75a77f28c5f6d8f33
mudir=mupdf-$(printf "%.7s" $mupdfrev)
mutgz=mupdf-$mupdfrev.tgz
muurl="http://git.ghostscript.com/?p=mupdf.git;a=snapshot;h=$mupdfrev;sf=tgz"

test -d lablGL-1.04 || (wget -nc $lablgl && tar -xzf lablgl-1.04.tar.gz)

test -e $mutgz || wget -nc $muurl -O $mutgz
test -e $(basename $mupdf3p) || wget -nc $mupdf3p
test -d $mudir || tar -xzf $(basename $mutgz)
test -d $mudir/thirdparty/openjpeg-1.5.0-patched || \
    unzip -o -d $mudir $(basename $mupdf3p)

executable_p() {
    command -v $1 >/dev/null 2>&1
}

executable_p gmake && make=gmake || make=make

(cd lablGL-1.04                                             \
    && sed 17d Makefile.config.linux.mdk > Makefile.config  \
    && $make -j 1 lib $(test "$1" = opt && echo libopt)     \
    && $make -j 1 install                                   \
            BINDIR=$root/bin                                \
            LIBDIR=$root/lib/ocaml                          \
            DLLDIR=$root/lib/ocaml/stublibs                 \
            INSTALLDIR=$root/lib/ocaml/lablGL)

(cd $mudir && $make -j "$jobs" build=release        \
    FREETYPE_DIR=thirdparty/freetype-2.4.10         \
    JBIG2DEC_DIR=thirdparty/jbig2dec                \
    JPEG_DIR=thirdparty/jpeg-9                      \
    OPENJPEG_DIR=thirdparty/openjpeg-1.5.0-patched  \
    ZLIB_DIR=thirdparty/zlib-1.2.7)

cd ..

srcpath=$(dirname $0)

sh $srcpath/mkhelp.sh $srcpath/keystoml.ml $srcpath/KEYS > help.ml

tp=$root/$mudir/thirdparty

ccopt="-O"
ccopt="$ccopt -I $tp/jbig2dec"
ccopt="$ccopt -I $tp/jpeg-8d"
ccopt="$ccopt -I $tp/freetype-2.4.10/include"
ccopt="$ccopt -I $tp/openjpeg-1.4/libopenjpeg"
ccopt="$ccopt -I $tp/zlib-1.2.5"
ccopt="$ccopt -I $root/$mudir/fitz -I $root/$mudir/pdf -I $root/$mudir/xps"
ccopt="$ccopt -I $root/$mudir/cbz"

ccopt="$ccopt -include $tp/freetype-2.4.10/include/ft2build.h -D_GNU_SOURCE"

cclib="$cclib -L$root/$mudir/build/release"
cclib="$cclib -lfitz"
cclib="$cclib -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype -lpthread"
cclib="$cclib -lX11"

echo Building llpp...
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
            parser.cmx                     \
            wsi.cmx                        \
            main.cmx
    }
else
    executable_p ocamlc.opt && comp=ocamlc.opt || comp=ocamlc
    cmsuf=cmo
    dolink() {
        $comp -custom -o llpp            \
            -I $root/lib/ocaml/lablGL    \
            str.cma unix.cma lablgl.cma  \
            link.o                       \
            -cclib "$cclib"              \
            help.cmo                     \
            parser.cmo                   \
            wsi.cmo                      \
            main.cmo
    }
fi

$comp -c -o link.o -ccopt "$ccopt" $srcpath/link.c
$comp -c -o help.$cmsuf help.ml
$comp -c -o wsi.cmi $srcpath/wsi.mli
$comp -c -o wsi.$cmsuf $srcpath/wsi.ml
$comp -c -o parser.$cmsuf $srcpath/parser.ml
$comp -c -o main.$cmsuf -I $root/lib/ocaml/lablGL $srcpath/main.ml
dolink

echo All done
