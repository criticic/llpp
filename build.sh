#!/bin/sh
set -ex
ccopt='-Wno-pointer-sign -O2'
mlopt='-warn-error +a -w +a -g -safe-string -I build'
if test -z "$native"; then
    comp=ocamlc.opt
    osu=.cmo
    asu=.cma
    lfl=-custom
else
    comp=ocamlopt.opt
    osu=.cmx
    asu=.cmxa
    lfl=
fi

mkdir -p build/lablGL
$comp -ccopt "$ccopt -o build/lablGL/ml_raw.o" -c lablGL/ml_raw.c
$comp -ccopt "$ccopt -o build/lablGL/ml_gl.o" -c lablGL/ml_gl.c
$comp -ccopt "$ccopt -o build/lablGL/ml_glarray.o" -c lablGL/ml_glarray.c
$comp -ccopt "-I mupdf/include -I mupdf/thirdparty/freetype/include -Wextra -Wall -Werror -D_GNU_SOURCE -O -g -std=c99 -pedantic-errors -Wunused-parameter -Wsign-compare -Wshadow -o build/link.o" -c link.c
/bin/sh mkhelp.sh KEYS unknown >build/help.ml
$comp -c -I build  -o build/help$osu build/help.ml
$comp -c $mlopt -o build/utils$osu utils.ml
$comp -c -I build/lablGL -o build/lablGL/raw.cmi lablGL/raw.mli
$comp -c -I build/lablGL -o build/lablGL/gl.cmi lablGL/gl.mli
$comp -c -I build/lablGL -o build/lablGL/glFunc.cmi lablGL/glFunc.mli
$comp -c -I build/lablGL -o build/lablGL/glMat.cmi lablGL/glMat.mli
$comp -c $mlopt -o build/parser$osu parser.ml
$comp -c -I build/lablGL -o build/lablGL/glClear.cmi lablGL/glClear.mli
$comp -c -I build/lablGL -o build/lablGL/glPix.cmi lablGL/glPix.mli
$comp -c -I build/lablGL -o build/lablGL/glDraw.cmi lablGL/glDraw.mli
$comp -c -I build/lablGL -o build/lablGL/glArray.cmi lablGL/glArray.mli
$comp -c -I build/lablGL -o build/lablGL/glTex.cmi lablGL/glTex.mli
$comp -c -I build/lablGL -o build/lablGL/glMisc.cmi lablGL/glMisc.mli
$comp -c $mlopt -o build/wsi.cmi wsi.mli
$comp -c $mlopt -I build/lablGL -o build/config$osu config.ml
$comp -c $mlopt -I build/lablGL -o build/main$osu -pp "sed -f pp.sed" main.ml
$comp -c -I build/lablGL -o build/lablGL/glDraw$osu lablGL/glDraw.ml
$comp -c -I build/lablGL -o build/lablGL/glArray$osu lablGL/glArray.ml
$comp -c -I build/lablGL -o build/lablGL/raw$osu lablGL/raw.ml
$comp -c -I build/lablGL -o build/lablGL/glClear$osu lablGL/glClear.ml
$comp -c $mlopt -o build/wsi$osu wsi.ml
$comp -c -I build/lablGL -o build/lablGL/gl$osu lablGL/gl.ml
$comp -c -I build/lablGL -o build/lablGL/glMat$osu lablGL/glMat.ml
$comp -c -I build/lablGL -o build/lablGL/glMisc$osu lablGL/glMisc.ml
$comp -c -I build/lablGL -o build/lablGL/glFunc$osu lablGL/glFunc.ml
$comp -c -I build/lablGL -o build/lablGL/glTex$osu lablGL/glTex.ml
$comp -c -I build/lablGL -o build/lablGL/glPix$osu lablGL/glPix.ml
$comp -g $lfl -I lablGL -o build/llpp unix$asu str$asu build/help$osu build/lablGL/raw$osu build/utils$osu build/parser$osu build/lablGL/glMisc$osu build/wsi$osu build/lablGL/gl$osu build/lablGL/glMat$osu build/lablGL/glFunc$osu build/lablGL/glClear$osu build/lablGL/glPix$osu build/lablGL/glTex$osu build/lablGL/glDraw$osu build/config$osu build/lablGL/glArray$osu build/main$osu build/link.o -cclib "-lGL -lX11 -lmupdf -lz -lfreetype -ljpeg -ljbig2dec -lopenjpeg -lmujs -lpthread -Lmupdf/build/native -lcrypto build/lablGL/ml_gl.o build/lablGL/ml_glarray.o build/lablGL/ml_raw.o"
