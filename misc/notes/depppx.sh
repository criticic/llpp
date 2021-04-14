#!/bin./sh
set -eu
D=$HOME/xsrc/llpp/

shell() {
    echo shell
    local t=/tmp/main.ppx
    cd $D
    ocamlc -depend -one-line -bytecode \
           -ppx 'tee $0 >$2 <$1' main.ml >/tmp/deps_shell
    ocamlc -c -I build -I build/wsi/x11 -I build/lablGL -o /tmp/main.cmo \
           -impl $t
}

direct() {
    echo direct
    cd $D
    ocamlc -depend -one-line -bytecode main.ml >/tmp/deps_direct
    ocamlc -c -I build -I build/wsi/x11 -I build/lablGL -o /tmp/main.cmo main.ml
}

C() {
    echo C
    local t=/tmp/main.ppx
    cd $D
    ocamlc -depend -one-line -bytecode \
           -ppx "$D/test/depppx $t" main.ml >/tmp/deps_C
    ocamlc -c -I build -I build/wsi/x11 -I build/lablGL -o /tmp/main.cmo \
           -impl $t
}

case "${1-}" in
    shell) shell;;
    direct) direct;;
    C) C;;
    *) direct;;
esac
