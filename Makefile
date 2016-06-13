MV = mv
RM = rm
SED = sed
GIT = git
CP = cp
OCAMLDEP = ocamldep
LLPP = llpp
OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
STDLIB = $(shell $(OCAMLC) -where)
SYSTEM = x11
# SYSTEM = cocoa

VERSION = $(or $(shell $(GIT) describe --tags 2>/dev/null),unknown)

LDFLAGS = -L mupdf/build/native
LDLIBS = -lmupdf -lmupdfthird -lpthread
CFLAGS = -I mupdf/include -I mupdf/thirdparty/freetype/include
OCAMLCFLAGS = -I lablGL

BEST = native
# BEST = byte

ifeq ($(SYSTEM),cocoa)
	CFLAGS += -D__COCOA__
else ifeq ($(shell uname),Darwin)
	LDFLAGS += -L/opt/X11/lib
	CFLAGS += -I/opt/X11/include
endif

all: $(LLPP)

wsi.ml: wsi_$(SYSTEM).ml
	$(RM) -f $@
	$(CP) $< $@

main.ml: main.mlp pp.sed
	$(SED) -f pp.sed $< > $@

help.ml: KEYS mkhelp.sh
	sh mkhelp.sh KEYS $(VERSION) > $@

lablGL/%.o: CFLAGS += -I $(STDLIB)

CFLAGS_LINK = \
	-Wextra -Wall -Werror \
	-D_GNU_SOURCE \
	-O -g -std=c99 -pedantic-errors -Wunused-parameter \
	-Wsign-compare -Wshadow

link.o: CFLAGS += $(CFLAGS_LINK) -I $(STDLIB)

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

$(LLPP)_x11.byte: $(O_FILES) $(addsuffix .cmo,$(LLPP_FILES))
	$(OCAMLC) -custom unix.cma str.cma $^ -cclib "$(LDFLAGS) $(LDLIBS) -lX11 -lGL" -o $@

$(LLPP)_x11.native: $(O_FILES) $(addsuffix .cmx,$(LLPP_FILES))
	$(OCAMLOPT) unix.cmxa str.cmxa $^ -cclib "$(LDFLAGS) $(LDLIBS) -lX11 -lGL" -o $@

main_osx.o: CFLAGS += -I $(STDLIB)

$(LLPP)_cocoa.byte: main_osx.o $(addsuffix .cmo,$(LLPP_FILES)) $(O_FILES)
	$(OCAMLC) -custom str.cma unix.cma -cclib "$(LDFLAGS) $(LDLIBS) -framework cocoa -framework opengl" -o $@ $^

$(LLPP)_cocoa.native: main_osx.o $(addsuffix .cmx,$(LLPP_FILES)) $(O_FILES)
	$(OCAMLOPT) str.cmxa unix.cmxa -cclib "$(LDFLAGS) $(LDLIBS) -framework cocoa -framework opengl" -o $@ $^

$(LLPP): $(LLPP)_$(SYSTEM).$(BEST)
	$(RM) -f $@
	$(MV) $< $@

.PHONY: mupdf force_mupdf
mupdf:
	test -d mupdf || $(GIT) clone git://git.ghostscript.com/mupdf --recursive && \
	$(MAKE) -C mupdf build=native XCFLAGS="$(CFLAGS)" XLIBS="$(LDFLAGS)"

force_mupdf:
	$(RM) -rf mupdf
	$(GIT) clone git://git.ghostscript.com/mupdf --recursive && \
	$(MAKE) -C mupdf build=native # XCFLAGS="$(CFLAGS)" XLIBS="$(LDFLAGS)"

.PHONY: clean
clean:
	$(RM) -f main.ml help.ml wsi.ml
	$(RM) -f *.cm* *.o
	$(RM) -f lablGL/*.cm* lablGL/*.o
	$(RM) -f $(LLPP)_cocoa.* $(LLPP)_x11.* $(LLPP)

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) $(OCAMLOPTFLAGS) -c $<

.PHONY: depend
depend: main.ml help.ml wsi.ml
	$(OCAMLDEP) -all -I lablGL $(addsuffix .ml,$(LLPP_FILES)) > .depend

include .depend
