#!/bin/sh
set -eu

bdir=build/test
cloneargs="--reference-if-able $HOME/x/rcs/git/mupdf"
CC='ccache gcc'
CXX='ccache g++'
export cloneargs CC CXX
mkdir -p $bdir
sh misc/getmupdf.sh $bdir/mupdf
exec bash build.bash $bdir
