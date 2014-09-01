#!/bin/sh
# mkcomp --- make completion files
#
# Usage: mkcomp <template file>

parse_infile () {
    # Output list of file extensions from all input files, one per line.
    # Delete lines beginning with `#', empty lines, and strip leading and
    # trailing whitespace.
    sed -f - <<EOF $@
/^#/d
/^[[:space:]]*$/d
s/^[[:space:]]+//g
s/[[:space:]]+$//g
EOF
}

print_regex () {
    # print regex of the file extensions from all input files
    # e.g. (foo|bar|baz)
    n=0

    printf '('
    parse_infile $@ | \
        while read -r line; do
            if test -n "$line"; then
                if test $n -eq 0; then
                    printf "%s" "$line"
                else
                    printf "|%s" "$line"
                fi
            fi
            n=$((n+1))
        done
    printf ')'
}

# combine llpp.in and llppac.in if making a llppac completion
f="$1"

if test "$(basename "$f")" = llppac; then
    llppac=llppac.in
fi
re="$(print_regex llpp.in $llppac)"

sed -e "s/@re@/$re/g" "$f.mk" > "$f"
printf "wrote: %s\n" "$f"
