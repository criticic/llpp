set -ex
ccopt='-Wno-pointer-sign -O2'
mlopt='-warn-error +a -w +a -g -safe-string -I build'
comp=ocamlc.opt

mkdir -p build/lablGL
$comp -ccopt "$ccopt -o build/lablGL/ml_raw.o" -c lablGL/ml_raw.c
$comp -ccopt "$ccopt -o build/lablGL/ml_gl.o" -c lablGL/ml_gl.c
$comp -ccopt "$ccopt -o build/lablGL/ml_glarray.o" -c lablGL/ml_glarray.c
$comp -ccopt "-I mupdf/include -I mupdf/thirdparty/freetype/include -Wextra -Wall -Werror -D_GNU_SOURCE -O -g -std=c99 -pedantic-errors -Wunused-parameter -Wsign-compare -Wshadow -o build/link.o" -c link.c
/bin/sh mkhelp.sh KEYS unknown >build/help.ml
$comp -c -I build  -o build/help.cmo build/help.ml
$comp -c $mlopt -o build/utils.cmo utils.ml
$comp -c -I build/lablGL -o build/lablGL/raw.cmi lablGL/raw.mli
$comp -c -I build/lablGL -o build/lablGL/gl.cmi lablGL/gl.mli
$comp -c -I build/lablGL -o build/lablGL/glFunc.cmi lablGL/glFunc.mli
$comp -c -I build/lablGL -o build/lablGL/glMat.cmi lablGL/glMat.mli
$comp -c $mlopt -o build/parser.cmo parser.ml
$comp -c -I build/lablGL -o build/lablGL/glClear.cmi lablGL/glClear.mli
$comp -c -I build/lablGL -o build/lablGL/glPix.cmi lablGL/glPix.mli
$comp -c -I build/lablGL -o build/lablGL/glDraw.cmi lablGL/glDraw.mli
$comp -c -I build/lablGL -o build/lablGL/glArray.cmi lablGL/glArray.mli
$comp -c -I build/lablGL -o build/lablGL/glTex.cmi lablGL/glTex.mli
$comp -c -I build/lablGL -o build/lablGL/glMisc.cmi lablGL/glMisc.mli
$comp -c $mlopt -o build/wsi.cmi wsi.mli
$comp -c $mlopt -I build/lablGL -o build/config.cmo config.ml
$comp -c $mlopt -I build/lablGL -o build/main.cmo -pp "sed -f pp.sed" main.ml
$comp -c -I build/lablGL -o build/lablGL/glDraw.cmo lablGL/glDraw.ml
$comp -c -I build/lablGL -o build/lablGL/glArray.cmo lablGL/glArray.ml
$comp -c -I build/lablGL -o build/lablGL/raw.cmo lablGL/raw.ml
$comp -c -I build/lablGL -o build/lablGL/glClear.cmo lablGL/glClear.ml
$comp -c $mlopt -o build/wsi.cmo wsi.ml
$comp -c -I build/lablGL -o build/lablGL/gl.cmo lablGL/gl.ml
$comp -c -I build/lablGL -o build/lablGL/glMat.cmo lablGL/glMat.ml
$comp -c -I build/lablGL -o build/lablGL/glMisc.cmo lablGL/glMisc.ml
$comp -c -I build/lablGL -o build/lablGL/glFunc.cmo lablGL/glFunc.ml
$comp -c -I build/lablGL -o build/lablGL/glTex.cmo lablGL/glTex.ml
$comp -c -I build/lablGL -o build/lablGL/glPix.cmo lablGL/glPix.ml
$comp -g -custom -I lablGL -o build/llpp unix.cma str.cma build/help.cmo build/lablGL/raw.cmo build/utils.cmo build/parser.cmo build/lablGL/glMisc.cmo build/wsi.cmo build/lablGL/gl.cmo build/lablGL/glMat.cmo build/lablGL/glFunc.cmo build/lablGL/glClear.cmo build/lablGL/glPix.cmo build/lablGL/glTex.cmo build/lablGL/glDraw.cmo build/config.cmo build/lablGL/glArray.cmo build/main.cmo build/link.o -cclib "-lGL -lX11 -lmupdf -lz -lfreetype -ljpeg -ljbig2dec -lopenjpeg -lmujs -lpthread -Lmupdf/build/native -lcrypto build/lablGL/ml_gl.o build/lablGL/ml_glarray.o build/lablGL/ml_raw.o"

