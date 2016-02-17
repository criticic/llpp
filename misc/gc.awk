#!/usr/bin/gawk -f
@load "filefuncs"

BEGIN {
    RS = OFS = ORS = "\000"
}

# courtesy freenode#awk/izabera
function quote(str) {
    gsub(/'/, "\\'", str);
    return str
}

{
    path = $0
    getline time

    qpath = quote(path)
    r = stat(path)
    data[1] = 1
    if (stat(path, data) == -1) {
        if ("locate -b -l 1 -e '/" qpath "$'" | getline npath > 0) {
            npath = ""
        }
        print path npath "\000";
        if (!npath) {
            print "Removing " qpath "\n" > "/dev/stderr"
        }
    }
}
