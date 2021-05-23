#!/bin/sh
set -eu

mkdir -p /tmp/cmi/{d,b/d}
cd /tmp/cmi
touch a.mli d/a.ml
ocamlc -c -I b/ -o b/a.cmi a.mli
ocamlc -c -I b/ -o b/d/a.cmo d/a.ml

# after running this script a.cmi will be in b/ _AND_ in b/d
