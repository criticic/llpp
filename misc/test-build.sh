#!/bin/sh

cloneargs='--reference /home/malc/x/rcs/git/mupdf'
CC='ccache gcc'
CXX='ccache g++'
export cloneargs CC CXX
sh misc/getmupdf.sh build-test/mupdf
exec bash build.bash build-test
