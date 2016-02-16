BEGIN {
    RS = OFS = ORS = "\000"
}

# http://stackoverflow.com/questions/29284472/awk-check-file-exists
function file_exists(file)
{
    return system ("test -e \"" file "\"") == 0
}

{
    path = $0
    getline time

    if (!file_exists(path)) {
        if ("locate -b -l 1 -e /\"" path "\"$" | getline npath > 0) {
            npath = ""
        }
        print path npath "\000";

        # http://www.linuxmisc.com/12-unix-shell/e099227d76b1246a.htm
        # not sold that it is portable - P1003.1 D3 does not metion this
        # print "Removing \"" path "\"\n" > "/dev/stderr"
    }
}
