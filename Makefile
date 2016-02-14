mupdf:
	test -d mupdf || git clone git://git.ghostscript.com/mupdf --recursive && \
	cd mupdf && \
	make build=native XCFLAGS=-I/usr/X11/include XLIBS=-L/opt/X11/lib

clean:
	rm -rf mupdf

.PHONY: mupdf clean
