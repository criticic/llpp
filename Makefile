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

CFLAGS = -I /opt/X11/include
LDFLAGS = -L /opt/X11/lib

LIBFLAGS := -L mupdf/build/native
INCFLAGS := -I mupdf/include -I mupdf/thirdparty/freetype/include
LIBS := -lGL -lX11 -lmupdf -lmupdfthird

main.ml: main.mlp pp.sed
	$(SED) -f pp.sed $< > $@

help.ml: KEYS mkhelp.sh
	sh mkhelp.sh KEYS $(VERSION) > $@

lablGL/%.o: lablGL/%.c
	$(CC) -I $(STDLIB) -o $@ -c $<

link.o: link.c
	$(CC) $(CFLAGS) $(INCFLAGS) -I $(STDLIB) -c $<

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
	$(OCAMLC) -custom -cclib "$(LDFLAGS) $(LIBFLAGS) $(LIBS)" -o $@ unix.cma str.cma $^

$(LLPP).o: $(addsuffix .cmo,$(LLPP_FILES))
	$(OCAMLC) -output-obj -o $@ unix.cma str.cma $^

$(LLPP).native: $(O_FILES) $(addsuffix .cmx,$(LLPP_FILES))
	$(OCAMLOPT) -cclib "$(LDFLAGS) $(LIBFLAGS) $(LIBS)" -o $@ unix.cmxa str.cmxa $^

$(LLPP).native.o: $(addsuffix .cmx,$(LLPP_FILES))
	$(OCAMLOPT) -output-obj -o $@ unix.cmxa str.cmxa $^

main_osx.o: main_osx.m
	$(CC) -I $(STDLIB) -fmodules -o $@ -c $<

llpp.osx: main_osx.o llpp.o $(O_FILES)
	$(CC) -L $(STDLIB) $(LDFLAGS) $(LIBFLAGS) $(LIBS) -lunix -lcamlstr -lcamlrun -framework cocoa -o $@ $^

llpp.native.osx: main_osx.o llpp.native.o $(O_FILES)
	$(CC) -L $(STDLIB) $(LDFLAGS) $(LIBFLAGS) $(LIBS) -lunix -lcamlstr -ltermcap -lasmrun -framework cocoa -o $@ $^

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
	$(MAKE) -C mupdf build=native XCFLAGS="$(SYS_INCLUDES)' XLIBS='-L /opt/X11/lib'

.PHONY: clean
clean:
	rm -f main.ml help.ml
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f lablGL/*.cmo lablGL/*.cmi lablGL/*.cmx lablGL/*.o
	rm -f $(LLPP).native $(LLPP) $(LLPP).native.o

include .depend
