srcpath=$(dirname $0)

mupdf=/home/malc/x/rcs/git/mupdf

mupdflibpath=$mupdf/build/release
mupdfincpath=$mupdf/fitz:$mupdf/pdf

cclib="-lmupdf -lfitz -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype"

export LIBRARY_PATH=$LIBRARY_PATH:$mupdflibpath
export CPATH=$CPATH:$mupdfincpath

ocamlc -c -o link.o -ccopt -O $srcpath/link.c
ocamlc -c -o main.cmo -I +lablGL $srcpath/main.ml
ocamlc -c -o parser.cmo $srcpath/parser.ml

ocamlc -custom -o llpp \
-I +lablGL \
str.cma unix.cma lablgl.cma lablglut.cma \
link.o \
-cclib "$cclib" \
parser.cmo \
main.cmo
