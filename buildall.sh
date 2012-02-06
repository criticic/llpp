# builds "hard" prerequisites and llpp
set -e

mkdir -p 3rdp
cd 3rdp
root=$(pwd)

lablgl=http://wwwfun.kurims.kyoto-u.ac.jp/soft/lsl/dist/lablgl-1.04.tar.gz
mupdf=git://git.ghostscript.com/mupdf.git
mupdf3p=http://mupdf.com/download/mupdf-thirdparty-2012-01-27.zip
mupdfrev=5cd45b68ad936006f3783abbcc064658d5e3508a

test -d lablGL-1.04 || (wget -nc $lablgl && tar -xzf lablgl-1.04.tar.gz)
if ! test -d mupdf; then
    wget -nc \
       "http://git.ghostscript.com/?p=mupdf.git;a=snapshot;h=$mupdfrev;sf=tgz" \
       -O mupdf-$mupdfrev.tgz
    tar xfz mupdf-$mupdfrev.tgz
fi

test -d mupdf/thirdparty || \
    (wget -nc $mupdf3p && unzip -d mupdf $(basename $mupdf3p))

make=$(gmake -v >/dev/null 2>&1 && echo gmake || echo make)

(cd lablGL-1.04 \
    && cat Makefile.config.linux.mdk > Makefile.config \
    && $make glut glutopt \
    && $make install \
            BINDIR=$root/bin \
            LIBDIR=$root/lib/ocaml \
            DLLDIR=$root/lib/ocaml/stublibs \
            INSTALLDIR=$root/lib/ocaml/lablGL)

(cd mupdf && $make build=release)

cd ..

srcpath=$(dirname $0)

sh mkhelp.sh $srcpath/keystoml.ml $srcpath/KEYS > help.ml

tp=$root/mupdf/thirdparty

ccopt="-O"
ccopt="$ccopt -I $tp/jbig2dec"
ccopt="$ccopt -I $tp/jpeg-8d"
ccopt="$ccopt -I $tp/freetype-2.4.8/include"
ccopt="$ccopt -I $tp/openjpeg-1.4/libopenjpeg"
ccopt="$ccopt -I $tp/zlib-1.2.5"
ccopt="$ccopt -I $root/mupdf/fitz -I $root/mupdf/pdf -I $root/mupdf/xps"
ccopt="$ccopt -I $root/mupdf/cbz"

ccopt="$ccopt -include ft2build.h -D_GNU_SOURCE"

cclib="$cclib -L$root/mupdf/build/release"
cclib="$cclib -lmupdf -lmuxps -lmucbz -lfitz"
cclib="$cclib -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype"
cclib="$cclib -lX11"

if test "$1" = "opt"; then
    cclib="$cclib -lpthread"
    ocamlopt -c -o link.o -ccopt "$ccopt" $srcpath/link.c
    ocamlopt -c -o help.cmx help.ml
    ocamlopt -c -o parser.cmx $srcpath/parser.ml
    ocamlopt -c -o main.cmx -I $root/lib/ocaml/lablGL $srcpath/main.ml

    ocamlopt -o llpp \
    -I $root/lib/ocaml/lablGL \
    str.cmxa unix.cmxa lablgl.cmxa lablglut.cmxa \
    link.o \
    -cclib "$cclib" \
    help.cmx \
    parser.cmx \
    main.cmx
else
    ocamlc -c -o link.o -ccopt "$ccopt" $srcpath/link.c
    ocamlc -c -o help.cmo help.ml
    ocamlc -c -o parser.cmo $srcpath/parser.ml
    ocamlc -c -o main.cmo -I $root/lib/ocaml/lablGL $srcpath/main.ml

    ocamlc -custom -o llpp \
        -I $root/lib/ocaml/lablGL \
        str.cma unix.cma lablgl.cma lablglut.cma \
        link.o \
        -cclib "$cclib" \
        help.cmo \
        parser.cmo \
        main.cmo
fi
