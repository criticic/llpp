# builds "hard" prerequisites and llpp
set -e

use_sumatrapdf_patched_mupdf=false

mkdir -p 3rdp
cd 3rdp

root=$(pwd)

openjpeg=http://openjpeg.googlecode.com/svn/trunk/
jbig2dec=git://git.ghostscript.com/jbig2dec.git
lablgl=:pserver:anoncvs@camlcvs.inria.fr:/caml
mupdf=git://git.ghostscript.com/mupdf.git
sumatrapdf=http://sumatrapdf.googlecode.com/svn/trunk

test -d openjpeg || svn -r r608 checkout $openjpeg openjpeg
test -d jbig2dec || git clone $jbig2dec jbig2dec
test -d lablgl   || cvs -d $lablgl co -d lablgl bazar-ocaml/lablGL

if ! test -d mupdf; then
    if $use_sumatrapdf_patched_mupdf; then
        svn checkout $sumatrapdf/mupdf mupdf
    else
        git clone $mupdf
    fi
fi

mkdir -p $root/bin
mkdir -p $root/lib
mkdir -p $root/include

(cd openjpeg \
    && make dist \
    && cp dist/*.h $root/include/ \
    && cp dist/*.a $root/lib/)

(cd jbig2dec \
    && make -f Makefile.unix install prefix=$root && rm -f $root/lib/*.so*)

(cd lablgl \
    && cat Makefile.config.linux.mdk > Makefile.config \
    && make glut glutopt \
    && make install \
            BINDIR=$root/bin \
            LIBDIR=$root/lib/ocaml \
            DLLDIR=$root/lib/ocaml/stublibs \
            INSTALLDIR=$root/lib/ocaml/lablGL)

export CPATH=$CPATH:$root/include:$root/mupdf/pdf:$root/mupdf/fitz
export LIBRARY_PATH=$LIBRARY_PATH:$root/lib:$root/mupdf/build/release

(cd mupdf && make build=release)

cd ..

srcpath=$(dirname $0)

sh mkhelp.sh $srcpath/keystoml.ml $srcpath/KEYS > help.ml

ccopt="$(freetype-config --cflags) -O -include ft2build.h"
if test "$1" = "opt"; then
    cclib="-lmupdf -lfitz -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype -lpthread"
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
    cclib="-lmupdf -lfitz -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype"
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
