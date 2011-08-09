srcpath=$(dirname $0)

mupdf=/home/malc/x/rcs/git/mupdf

mupdflibpath=$mupdf/build/release
mupdfincpath=$mupdf/fitz:$mupdf/pdf

cclib="-lmupdf -lfitz -lz -ljpeg -lopenjpeg -ljbig2dec -lfreetype"

export LIBRARY_PATH=$LIBRARY_PATH:$mupdflibpath
export CPATH=$CPATH:$mupdfincpath

sh mkhelp.sh $srcpath/keystoml.ml $srcpath/KEYS > help.ml

ocamlc -c -o link.o -ccopt -O $srcpath/link.c
ocamlc -c -o help.cmo help.ml
ocamlc -c -o parser.cmo $srcpath/parser.ml
ocamlc -c -o main.cmo -I +lablGL $srcpath/main.ml

ocamlc -custom -o llpp \
-I +lablGL \
str.cma unix.cma lablgl.cma lablglut.cma \
link.o \
-cclib "$cclib" \
help.cmo \
parser.cmo \
main.cmo
