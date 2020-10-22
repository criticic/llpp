#!/bin/sh
set -eu

# On why wsi.mli is symlinked from the root to wsi/_platform_/wsi.mli

mkdir -p /tmp/cmi/{d,b/d}
cd /tmp/cmi
touch a.mli d/a.ml
ocamlc -c -I b/ -o b/a.cmi a.mli
ocamlc -c -I b/ -o b/d/a.cmo d/a.ml

# after running this script a.cmi will be in b/ and in b/d
