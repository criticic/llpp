#!/usr/bin/gawk -f
@load "filefuncs"

BEGIN {
    RS = OFS = ORS = "\000"
}

{
    path = $0
    getline time

    r = stat(path)
    data[1] = 1
    if (stat(path, data) == -1) {
        if ("locate -b -l 1 -e \"/" path "$\"" | getline npath > 0) {
            npath = ""
        }
        print path npath "\000";
        if (!npath) {
            print "Removing " path "\n" > "/dev/stderr"
        }
    }
}
