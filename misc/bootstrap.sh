#!/bin/sh
set -e

executable_p() { command -v "$1" >/dev/null 2>&1; }
die() { echo "$1" >&2; exit 1; }

if executable_p wget; then dl() { wget -q $1 -O $2; }
elif executable_p curl; then dl() { curl $1 -o $2; }
else die "no program to fetch remote urls found"
fi

mkdir -p bootstrap
prefix=$PWD/bootstrap

false && {
    xz=http://caml.inria.fr/pub/distrib/ocaml-4.06/ocaml-4.06.1.tar.xz
    test -e ocaml-4.06.1.tar.xz || dl $xz ocaml-4.06.0.1.xz
    tar xf ocaml-4.06.0.1.xz
    cd ocaml-4.06.1
    ./configure -prefix $prefix
    make -j4 world -s
    make install
    cd ..
}

false && {
    rmudir=$HOME/x/rcs/git/mupdf
    test -e $rmudir || ref= && ref="--reference $rmudir"
    test -e mupdf || git clone $ref git://git.ghostscript.com/mupdf.git
    make -C mupdf build=native -j4
}

git clone git://repo.or.cz/llpp.git
cd llpp
ln -s ../mupdf
PATH=$prefix/bin:$PATH sh ./build.sh build
