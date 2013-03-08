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
baseurl="http://git.ghostscript.com/"

mupdfrev=1b4b5fdfa6b74827631d42efd2a61226125f036b
mudir=mupdf-$(printf "%.7s" $mupdfrev)
mutgz=mupdf-$mupdfrev.tgz
muurl="${baseurl}?p=mupdf.git;a=snapshot;h=$mupdfrev;sf=tgz"

test -d lablGL-1.04 || (wget -nc $lablgl && tar -xzf lablgl-1.04.tar.gz)
test -e $mutgz || wget -nc $muurl -O $mutgz
test -d $mudir || tar -xzf $mutgz

while read m r; do
    d=$m-$(printf "%.7s" $r)
    t=$m-$r.tgz
    p=$m
    test $m = jbig2dec || p=thirdparty/$m
    u="${baseurl}?p=$p.git;a=snapshot;h=$r;sf=tgz"
    test -e $t || wget -nc $u -O $t
    test -e $mudir/thirdparty/$m/README ||
    (rm -fr $mudir/thirdparty/$m && tar -xzf $t && mv $d $mudir/thirdparty/$m)
done <<-EOF
freetype 2ef0a19842ae1172bec153225328aaaeaf130a18
jbig2dec 3e6c1b0670740be3b138228dcc134bf5e6c1eceb
jpeg 219d59dcfd0e6ce8a3d8c5510e29237f0b5078ed
openjpeg d5693f4ec8635d81defc92619c02134b6b785b06
zlib c16b1b18ddaaf090caf321af831bccac6381a381
EOF

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

(cd $mudir && $make -j "$jobs" build=release)

cd ..

srcpath=$(dirname $0)

tp=$root/$mudir/thirdparty

ccopt="-O"
ccopt="$ccopt -I $tp/jbig2dec"
ccopt="$ccopt -I $tp/jpeg"
ccopt="$ccopt -I $tp/freetype/include"
ccopt="$ccopt -I $tp/openjpeg/libopenjpeg"
ccopt="$ccopt -I $tp/zlib"
ccopt="$ccopt -I $root/$mudir/fitz -I $root/$mudir/pdf -I $root/$mudir/xps"
ccopt="$ccopt -I $root/$mudir/cbz"

ccopt="$ccopt -include $tp/freetype/include/ft2build.h -D_GNU_SOURCE"

cclib="$cclib -L$root/$mudir/build/release"
cclib="$cclib -lfitz"
cclib="$cclib -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype -lpthread"
cclib="$cclib -lX11"

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
        $comp -custom -o llpp            \
            -I $root/lib/ocaml/lablGL    \
            str.cma unix.cma lablgl.cma  \
            link.o                       \
            -cclib "$cclib"              \
            help.cmo                     \
            utils.cmo                    \
            parser.cmo                   \
            wsi.cmo                      \
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
