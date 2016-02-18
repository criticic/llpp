LN := ln
RM := rm
SED := sed
OCAMLDEP := ocamldep
OCAMLBUILD := ocamlbuild -classic-display
LLPP := llpp
OCAMLC := ocamlc.opt
OCAMLOPT := ocamlopt.opt
STDLIB := $(shell $(OCAMLC) -where)

VERSION := $(or $(shell git describe --tags 2>/dev/null),unknown)

main.ml: main.mlp pp.sed
	$(SED) -f pp.sed $< > $@

help.ml: KEYS mkhelp.sh
	sh mkhelp.sh KEYS $(VERSION) > $@

lablGL/%.o: lablGL/%.c
	$(CC) -I $(STDLIB) -o $@ -c $<

link.o: link.c
	$(CC) -I /opt/X11/include -I $(STDLIB) -I mupdf/include -I mupdf/thirdparty/freetype/include -c $<

LLPP_FILES = \
	lablGL/gl \
	lablGL/raw \
	lablGL/glPix \
	lablGL/glDraw \
	lablGL/glMat \
	lablGL/glMisc \
	lablGL/glFunc \
	lablGL/glTex \
	lablGL/glArray \
	lablGL/glClear \
	help \
	utils \
	parser \
	wsi \
	config \
	main

O_FILES = \
	link.o \
	lablGL/ml_raw.o \
	lablGL/ml_gl.o \
	lablGL/ml_glarray.o

$(LLPP): $(O_FILES) $(addsuffix .cmo,$(LLPP_FILES))
	$(OCAMLC) -custom -cclib '-L /opt/X11/lib -lGL -lX11' -cclib '-L mupdf/build/native -lmupdf -lmupdfthird' -o $@ unix.cma str.cma $^

$(LLPP).o: $(addsuffix .cmo,$(LLPP_FILES))
	$(OCAMLC) -output-obj -cclib '-L /opt/X11/lib -lGL -lX11' -cclib '-L mupdf/build/native -lmupdf -lmupdfthird' -o $@ unix.cma str.cma $^

$(LLPP).native: $(O_FILES) $(addsuffix .cmx,$(LLPP_FILES))
	$(OCAMLOPT) -cclib '-L /opt/X11/lib -lGL -lX11' -cclib '-L mupdf/build/native -lmupdf -lmupdfthird' -o $@ unix.cmxa str.cmxa $^

$(LLPP).native.o: $(addsuffix .cmx,$(LLPP_FILES))
	$(OCAMLOPT) -output-obj -cclib '-L /opt/X11/lib -lGL -lX11' -cclib '-L mupdf/build/native -lmupdf -lmupdfthird' -o $@ unix.cmxa str.cmxa $^

main_osx.o: main_osx.m
	$(CC) -I $(STDLIB) -fmodules -o $@ -c $<

llpp.osx: main_osx.o llpp.o $(O_FILES)
	$(CC) -L $(STDLIB) -L mupdf/build/native -L /opt/X11/lib -lGL -lX11 -lmupdf -lmupdfthird -lunix -lcamlstr -ltermcap -lcamlrun -framework cocoa -o $@ $^

llpp.native.osx: main_osx.o llpp.native.o $(O_FILES)
	$(CC) -L $(STDLIB) -L mupdf/build/native -L /opt/X11/lib -lGL -lX11 -lmupdf -lmupdfthird -lunix -lcamlstr -ltermcap -lasmrun -framework cocoa -o $@ $^

.PHONY: depend
depend: main.ml help.ml
	$(OCAMLDEP) -all -I lablGL *.ml lablGL/*.ml > .depend

%.cmi: %.mli
	$(OCAMLC) -I lablGL -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -I lablGL -c $<

%.cmx: %.ml
	$(OCAMLOPT) -I lablGL -c $<

.PHONY: mupdf
mupdf:
	test -d mupdf || git clone git://git.ghostscript.com/mupdf --recursive && \
	$(MAKE) -C mupdf build=native XCFLAGS='-I /opt/X11/include' XLIBS='-L /opt/X11/lib'

.PHONY: clean
clean:
	rm -f main.ml help.ml
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f lablGL/*.cmo lablGL/*.cmi lablGL/*.cmx lablGL/*.o
	rm -f $(LLPP).native $(LLPP) $(LLPP).native.o

include .depend
