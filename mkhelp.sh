#!/bin/sh
set -e
v=$(cd $(dirname $0) && git describe --tags --dirty 2>/dev/null || echo unknown)
sed "$1" -f - <<EOF
1i\let keys = [
s;\([^[:space:]]*\) *- \(.*\);\1\\\\t\2;
s;-----\(.*\)-----;\\xc2\\xb7\1;
s;";\\\\";g
s;^;";
s.$.";.
\$a\];; let version = "$v";;
EOF
