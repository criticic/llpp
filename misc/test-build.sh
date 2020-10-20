#!/bin/sh
set -eu

bdir=build/test
export CC="${CC:-ccache gcc}"
export CXX="${CXX:-ccache g++}"
mkdir -p $bdir
cloneargs="--reference-if-able $HOME/x/rcs/git/mupdf" \
         sh misc/getmupdf.sh $bdir/mupdf
exec bash build.bash $bdir
