srcpath=$(dirname $0)

mupdf=/home/malc/x/rcs/svn/sumatrapdf-read-only/mupdf

mupdflibpath=$mupdf/build/release
mupdfincpath=$mupdf

cclib="-lmupdf -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype"

export LIBRARY_PATH=$LIBRARY_PATH:$mupdflibpath
export CPATH=$CPATH:$mupdfincpath

ocamlc -c -o link.o -ccopt -O $srcpath/link.c
ocamlc -c -o main.cmo -I +lablGL $srcpath/main.ml

ocamlc -custom -o llpp \
-I +lablGL \
unix.cma lablgl.cma lablglut.cma \
link.o \
-cclib "$cclib" \
main.cmo
