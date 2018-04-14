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

true && {
    xz=http://caml.inria.fr/pub/distrib/ocaml-4.06/ocaml-4.06.1.tar.xz
    test -e ocaml-4.06.1.tar.xz || dl $xz ocaml-4.06.1.xz
    tar xf ocaml-4.06.1.xz
    cd ocaml-4.06.1
    ./configure -prefix $prefix
    make -j4 world -s
    make install
    cd ..
}

true && {
    rmudir=$HOME/x/rcs/git/mupdf
    test -d $rmudir || ref= && ref="--reference $rmudir"
    test -e mupdf || {
        git clone --recursive $ref git://git.ghostscript.com/mupdf.git
    } && {
        cd mupdf
        git pull
        cd -
    }
    make -C mupdf build=native -j4 libs
}

test -e llpp || git clone git://repo.or.cz/llpp.git && {
        cd llpp
        git pull
        cd -
    }
ln -sf $PWD/mupdf llpp
cd llpp
PATH=$prefix/bin:$PATH sh ./build.sh build
