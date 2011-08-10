ocaml $1 $2
v=$(git describe --tags --dirty || echo "unknown")
printf 'let version ="%s";;\n' $v
