# builds "hard" prerequisites and llpp
set -e

mkdir -p 3rdp
cd 3rdp

root=$(pwd)

openjpeg=http://openjpeg.googlecode.com/svn/trunk/
jbig2dec=git://git.ghostscript.com/jbig2dec.git
sumatrapdf=http://sumatrapdf.googlecode.com/svn/trunk/

test -d openjpeg || svn checkout $openjpeg openjpeg
test -d jbig2dec || git clone $jbig2dec jbig2dec
test -d mupdf    || svn checkout $sumatrapdf/mupdf mupdf

mkdir -p $root/include
mkdir -p $root/lib

(cd openjpeg \
    && make dist \
    && cp dist/*.h $root/include/ \
    && cp dist/*.a $root/lib/)

(cd jbig2dec \
    && (test -f Makefile || (test -f configure || sh autogen.sh --prefix=$root \
                             && ./configure --prefix=$root)) \
    && make install && rm $root/lib/*.so*)

# OCaml has no cross compiler so uname is sufficient
case $(uname -m) in
    ppc*|sparc*|arm*)
    mv mupdf/Makerules aaa && grep -vi "x86" aaa > mupdf/Makerules
    rm aaa
    ;;
    *)
    ;;
esac

(cd mupdf && make build=release)

cd ..

export CPATH=$CPATH:$root/include:$root/mupdf/mupdf:$root/mupdf/fitz
export LIBRARY_PATH=$CPATH:$root/lib:$root/mupdf/build/release

srcpath=$(dirname $0)

cclib="-lmupdf -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype"
ocamlc -c -o link.o -ccopt -O $srcpath/link.c
ocamlc -c -o main.cmo -I +lablGL $srcpath/main.ml

ocamlc -custom -o llpp \
-I +lablGL \
str.cma unix.cma lablgl.cma lablglut.cma \
link.o \
-cclib "$cclib" \
main.cmo
