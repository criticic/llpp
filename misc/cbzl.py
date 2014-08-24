#!/usr/bin/env python
# based on http://doeidoei.wordpress.com/2010/11/23/compressing-files-with-python-symlink-trouble/
import os, sys, zipfile

if len (sys.argv) < 2:
    print ("""\
usage: find . -name \*.jpg -or -name \*.png -print0 | python cbzl.py curimgs.cbz
   or: locate -0 .jpg | python cbzl.py alljpegs.cbz
""")
    sys.exit (1)

with zipfile.ZipFile (sys.argv[1], "w") as z:
    zerosep = sys.stdin.read ()
    files = zerosep.split ('\0')
    for name in files:
        a = zipfile.ZipInfo ()
        a.filename = os.path.basename (name)
        a.create_system = 3
        a.external_attr = 2716663808
        z.writestr (a, name)
