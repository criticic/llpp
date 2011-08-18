ocaml str.cma $1 $2
v=$(cd $(dirname $1) && git describe --tags --dirty 2>/dev/null \
    || echo "unknown")
printf 'let version ="%s";;\n' $v
