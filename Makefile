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

# CFLAGS = -I /opt/X11/include
# LDFLAGS = -L /opt/X11/lib

LIBFLAGS := -L mupdf/build/native
INCFLAGS := -I mupdf/include -I mupdf/thirdparty/freetype/include
LIBS := -lmupdf -lmupdfthird -lpthread # -lGL -lX11

all: $(LLPP).native.osx

main.ml: main.mlp pp.sed
	$(SED) -f pp.sed $< > $@

help.ml: KEYS mkhelp.sh
	sh mkhelp.sh KEYS $(VERSION) > $@

lablGL/%.o: lablGL/%.c
	$(CC) -I $(STDLIB) -o $@ -c $<

LINK_CFLAGS := \
#	-Wextra -Wall -Werror
	-D_GNU_SOURCE \
	-O -g -std=c99 -pedantic-errors -Wunused-parameter \
	-Wsign-compare -Wshadow

link.o: link.c
	$(CC) -I $(STDLIB) $(CFLAGS) $(INCFLAGS) $(LINK_CFLAGS) -c $<

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
	wsi_osx \
	config \
	main

O_FILES = \
	link.o \
	lablGL/ml_raw.o \
	lablGL/ml_gl.o \
	lablGL/ml_glarray.o

$(LLPP): $(O_FILES) $(addsuffix .cmo,$(LLPP_FILES))
	$(OCAMLC) -custom unix.cma str.cma $^ -cclib "$(LDFLAGS) $(LIBFLAGS) $(LIBS)" -o $@

# $(LLPP).o: $(addsuffix .cmo,$(LLPP_FILES))
# 	$(OCAMLC) -output-obj -o $@ unix.cma str.cma $^

$(LLPP).native: $(O_FILES) $(addsuffix .cmx,$(LLPP_FILES))
	$(OCAMLOPT) unix.cmxa str.cmxa $^ -cclib "$(LDFLAGS) $(LIBFLAGS) $(LIBS)" -o $@

# $(LLPP).native.o: $(addsuffix .cmx,$(LLPP_FILES))
# 	$(OCAMLOPT) $@ unix.cmxa str.cmxa -c $^

main_osx.o: main_osx.m
	$(CC) -I $(STDLIB) -o $@ -c $<

# $(LLPP).osx: main_osx.o llpp.o $(O_FILES)
# 	$(CC) -L $(STDLIB) $(LDFLAGS) $(LIBFLAGS) $(LIBS) -lunix -lcamlstr -lcamlrun -framework cocoa -o $@ $^

$(LLPP).native.osx: main_osx.o $(addsuffix .cmx,$(LLPP_FILES)) $(O_FILES)
	$(OCAMLOPT) str.cmxa unix.cmxa -cclib "$(LDFLAGS) $(LIBFLAGS) $(LIBS) -framework cocoa -framework opengl" -o $@ $^

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
	$(MAKE) -C mupdf build=native XCFLAGS="$(CFLAGS)" XLIBS="$(LDFLAGS)"

.PHONY: clean
clean:
	rm -f main.ml help.ml
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f lablGL/*.cmo lablGL/*.cmi lablGL/*.cmx lablGL/*.o
	rm -f $(LLPP).native $(LLPP) $(LLPP).native.o

include .depend
