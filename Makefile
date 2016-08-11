MV = mv
RM = rm
SED = sed
GIT = git
CP = cp
LN = ln
OCAMLDEP = ocamldep
LLPP = llpp
OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
STDLIB = $(shell $(OCAMLC) -where)
MUPDF_COMMIT = c1901196a057f9f8e46f9b4b97e7822a0fb3ef23

ifeq ($(shell uname),Darwin)
SYSTEM ?= cocoa
else
SYSTEM ?= x11
endif

GIT_DESCR = $(or $(shell $(GIT) describe --tags 2>/dev/null),unknown)
VERSION = $(shell echo $(GIT_DESCR) | sed -n 's/v\([0-9]*\).*/\1/p')

LDFLAGS = -L mupdf/build/native
LDLIBS = -lmupdf -lmupdfthird -lpthread
CFLAGS = -I mupdf/include -I mupdf/thirdparty/freetype/include
CFLAGS += -Wno-pointer-sign
OCAMLCFLAGS = -I lablGL

BEST = native
# BEST = byte

ifeq ($(SYSTEM),cocoa)
	CFLAGS += -D__COCOA__
	LDLIBS += -framework Cocoa -framework OpenGL
else
	LDLIBS += -lX11 -lGL
ifeq ($(shell uname),Darwin)
	LDFLAGS += -L/opt/X11/lib
	CFLAGS += -I/opt/X11/include
endif
endif

all: $(LLPP)

wsi.ml: wsi_$(SYSTEM).ml
	$(RM) -f $@
	$(CP) $< $@

help.ml: KEYS mkhelp.sh
	sh mkhelp.sh KEYS $(GIT_DESCR) > $@

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
	keys \
	wsi \
	config \
	main

O_FILES = \
	link.o \
	lablGL/ml_raw.o \
	lablGL/ml_gl.o \
	lablGL/ml_glarray.o

$(LLPP)_x11.byte: $(O_FILES) $(addsuffix .cmo,$(LLPP_FILES))
	$(OCAMLC) -custom unix.cma str.cma $^ -cclib "$(LDFLAGS) $(LDLIBS)" -o $@

$(LLPP)_x11.native: $(O_FILES) $(addsuffix .cmx,$(LLPP_FILES))
	$(OCAMLOPT) unix.cmxa str.cmxa $^ -cclib "$(LDFLAGS) $(LDLIBS)" -o $@

main_osx.o: CFLAGS += -I $(STDLIB)
main_osx.o: CC=$(shell ocamlc -config | grep bytecomp_c_co | cut -d: -f2)

$(LLPP)_cocoa.byte: main_osx.o $(addsuffix .cmo,$(LLPP_FILES)) $(O_FILES)
	$(OCAMLC) -custom str.cma unix.cma -cclib "$(LDFLAGS) $(LDLIBS)" -o $@ $^

$(LLPP)_cocoa.native: main_osx.o $(addsuffix .cmx,$(LLPP_FILES)) $(O_FILES)
	$(OCAMLOPT) str.cmxa unix.cmxa -cclib "$(LDFLAGS) $(LDLIBS)" -o $@ $^

$(LLPP): $(LLPP)_$(SYSTEM).$(BEST)
	$(RM) -f $@
	$(LN) $< $@

.PHONY: $(LLPP).app
$(LLPP).app: $(LLPP)_cocoa.$(BEST)
	$(RM) -rf $(LLPP).app
	mkdir -p $(LLPP).app/Contents/MacOS
	cat misc/Info.plist | sed s/@VERSION@/$(VERSION)/ | \
	sed s/@BUNDLE_VERSION@/$(GIT_DESCR)/ > $(LLPP).app/Contents/Info.plist
	cp $(LLPP)_cocoa.$(BEST) $(LLPP).app/Contents/MacOS/$(LLPP)

.PHONY: mupdf force_mupdf
mupdf:
	test -d mupdf || $(GIT) clone git://git.ghostscript.com/mupdf --recursive && \
	cd mupdf && \
	$(GIT) checkout $(MUPDF_COMMIT) && \
	$(MAKE) build=native libs XCFLAGS="$(CFLAGS)" XLIBS="$(LDFLAGS)"

force_mupdf:
	$(RM) -rf mupdf
	$(GIT) clone git://git.ghostscript.com/mupdf --recursive && \
	cd mupdf && \
	$(GIT) checkout $(MUPDF_COMMIT) && \
	$(MAKE) build=native libs XCFLAGS="$(CFLAGS)" XLIBS="$(LDFLAGS)"

.PHONY: clean
clean:
	$(RM) -f help.ml wsi.ml
	$(RM) -f *.cm* *.o
	$(RM) -f lablGL/*.cm* lablGL/*.o
	$(RM) -f $(LLPP)_cocoa.* $(LLPP)_x11.* $(LLPP)
	$(RM) -rf $(LLPP).app

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLCFLAGS) $(OCAMLOPTFLAGS) -c $<

.PHONY: depend
depend: wsi.ml help.ml
	$(OCAMLDEP) -I lablGL lablGL/*.mli lablGL/*.ml *.mli *.ml > .depend

include .depend
