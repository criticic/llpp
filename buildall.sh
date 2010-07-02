# builds "hard" prerequisites and llpp
set -e

use_sumatrapdf_patched_mupdf=true

mkdir -p 3rdp
cd 3rdp

root=$(pwd)

openjpeg=http://openjpeg.googlecode.com/svn/trunk/
jbig2dec=git://git.ghostscript.com/jbig2dec.git
lablgl=:pserver:anoncvs@camlcvs.inria.fr:/caml
mupdf=http://mupdf.com/download/snapshots/mupdf-r1300.tar.gz
sumatrapdf=http://sumatrapdf.googlecode.com/svn/trunk

test -d openjpeg || svn checkout $openjpeg openjpeg
test -d jbig2dec || git clone $jbig2dec jbig2dec
test -d lablgl   || cvs -d $lablgl co -d lablgl bazar-ocaml/lablGL

if ! test -d mupdf; then
    if $use_sumatrapdf_patched_mupdf; then
        svn checkout $sumatrapdf/mupdf mupdf && grep
        case $(uname -m) in
            ppc*|sparc*|arm*)
            mv mupdf/Makerules aaa && grep -vi "x86" aaa > mupdf/Makerules
            rm aaa
            ;;
            *)
            ;;
        esac
    else
        wget $mupdf && tar xf $(basename $mupdf)
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
    && (test -f Makefile || (test -f configure || sh autogen.sh --prefix=$root \
                             && ./configure --prefix=$root)) \
    && make install && rm -f $root/lib/*.so*)

(cd lablgl \
    && cat Makefile.config.linux.mdk > Makefile.config \
    && make glut glutopt \
    && make install \
            BINDIR=$root/bin \
            LIBDIR=$root/lib/ocaml \
            DLLDIR=$root/lib/ocaml/stublibs \
            INSTALLDIR=$root/lib/ocaml/lablGL)

export CPATH=$CPATH:$root/include:$root/mupdf/mupdf:$root/mupdf/fitz
export LIBRARY_PATH=$LIBRARY_PATH:$root/lib:$root/mupdf/build/release

(cd mupdf && make build=release)

cd ..

srcpath=$(dirname $0)

cclib="-lmupdf -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype"
ocamlc -c -o link.o -ccopt -O $srcpath/link.c
ocamlc -c -o main.cmo -I $root/lib/ocaml/lablGL $srcpath/main.ml

ocamlc -custom -o llpp \
-I $root/lib/ocaml/lablGL \
str.cma unix.cma lablgl.cma lablglut.cma \
link.o \
-cclib "$cclib" \
main.cmo
