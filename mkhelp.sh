#!/bin/sh
set -e
cat $2 | ocaml str.cma $1
v=$(cd $(dirname $1) && git describe --tags --dirty 2>/dev/null \
    || echo "unknown")
echo "let version =\"$v\";;\n"
