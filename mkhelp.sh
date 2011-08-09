ocaml $1 $2
v=$(git describe --tags || echo "unknown")
printf 'let version ="%s";;\n' $v
