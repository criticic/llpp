BUILDDIR ?= build
LABLGL_BUILDDIR = $(BUILDDIR)/lablGl
CCOPT = $(CFLAGS) -Wno-pointer-sign -O2
MLOPT = -warn-error +a -w +a -g -safe-string
# SRCDIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
VERSION = $(or $(shell git describe --tags 2>/dev/null),unknown)
MLOPTGL = -I lablGl -I $(BUILDDIR)/lablGl
NATIVE =

ifeq ($(shell uname),Darwin)
LIBGL_LFLAGS = -L/opt/X11/lib
LIBGL_CFLAGS = -I/opt/X11/include
else
LIBGL_LFLAGS =
LIBGL_CFLAGS =
endif

ifndef NATIVE
COMP = ocamlc.opt
OSU = .cmo
ASU = .cma
LFL = -custom
else
COMP = ocamlopt.opt
OSU = .cmx
ASU = .cmxa
LFL =
endif

$(info VERSION = $(VERSION))
$(info BUILDDIR = $(BUILDDIR))
$(info COMP = $(COMP))
$(info LIBGL_LFLAGS = $(LIBGL_LFLAGS))
$(info LIBGL_CFLAGS = $(LIBGL_CFLAGS))

all: $(BUILDDIR)/llpp

$(LABLGL_BUILDDIR):
	mkdir -p $@

LABLGL_FILES = \
	ml_raw.o ml_gl.o ml_glarray.o \
	gl$(OSU) raw$(OSU) glPix$(OSU) glDraw$(OSU) \
	glTex.cmi glMisc.cmi glMat$(OSU) glMisc$(OSU) \
	glFunc$(OSU) glTex$(OSU) glArray$(OSU) glClear$(OSU)

LABLGL_C_FILES = $(filter %.o,$(LABLGL_FILES))
LABLGL_MLI_FILES = $(filter %.cmi,$(LABLGL_FILES))
LABLGL_ML_FILES = $(filter %$(OSU),$(LABLGL_FILES))

$(addprefix $(LABLGL_BUILDDIR)/,$(LABLGL_C_FILES)): $(LABLGL_BUILDDIR)/%.o: lablGl/%.c
	$(COMP) -ccopt '$(CCOPT) -o $@' -c $<

$(addprefix $(LABLGL_BUILDDIR)/,$(LABLGL_MLI_FILES)): $(LABLGL_BUILDDIR)/%.cmi: lablGl/%.mli
	$(COMP) $(MLOPTGL) -o $@ -c $<

$(addprefix $(LABLGL_BUILDDIR)/,$(LABLGL_ML_FILES)): $(LABLGL_BUILDDIR)/%$(OSU): lablGl/%.ml
	$(COMP) $(MLOPTGL) -o $@ -c $<

lablGl: $(LABLGL_BUILDDIR) $(addprefix $(LABLGL_BUILDDIR)/,$(LABLGL_FILES))

$(BUILDDIR)/link.o: link.c mupdf
	$(COMP) -ccopt '$(LIBGL_CFLAGS) -I mupdf/include -I mupdf/thirdparty/freetype/include -Wextra -Wall -Werror -D_GNU_SOURCE -O -g -std=c99 -pedantic-errors -Wunused-parameter -Wsign-compare -Wshadow -o $@' -c $<

$(BUILDDIR)/help.ml: KEYS
	. mkhelp.sh $< > $@

$(BUILDDIR)/help$(OSU): $(BUILDDIR)/help.ml
	$(COMP) -o $@ -c $<

$(BUILDDIR)/utils$(OSU): utils.ml
	$(COMP) $(MLOPT) -o $@ -c $<

$(BUILDDIR)/parser$(OSU): parser.ml
	$(COMP) $(MLOPT) -o $@ -c $<

$(BUILDDIR)/wsi.cmi: wsi.mli
	$(COMP) $(MLOPT) -I $(BUILDDIR) -o $@ -c $<

$(BUILDDIR)/wsi$(OSU): wsi.ml $(BUILDDIR)/wsi.cmi
	$(COMP) $(MLOPT) -I $(BUILDDIR) -o $@ -c $<

$(BUILDDIR)/config$(OSU): config.ml $(BUILDDIR)/wsi$(OSU) $(BUILDDIR)/help$(OSU) $(BUILDDIR)/parser$(OSU)
	$(COMP) $(MLOPTGL) -I $(BUILDDIR) -o $@ -c $<

$(BUILDDIR)/main.ml: main.ml pp.sed
	sed -f pp.sed $< >$@

$(BUILDDIR)/main$(OSU): $(BUILDDIR)/main.ml $(BUILDDIR)/utils$(OSU) $(BUILDDIR)/config$(OSU)
	$(COMP) $(MLOPTGL) -I $(BUILDDIR) -o $@ -c $<

LLPP_FILES = \
	$(BUILDDIR)/help$(OSU) \
	$(LABLGL_BUILDDIR)/raw$(OSU) \
	$(BUILDDIR)/utils$(OSU) \
	$(BUILDDIR)/parser$(OSU) \
	$(LABLGL_BUILDDIR)/glMisc$(OSU) \
	$(BUILDDIR)/wsi$(OSU) \
	$(LABLGL_BUILDDIR)/gl$(OSU) \
	$(LABLGL_BUILDDIR)/glMat$(OSU) \
	$(LABLGL_BUILDDIR)/glFunc$(OSU) \
	$(LABLGL_BUILDDIR)/glClear$(OSU) \
	$(LABLGL_BUILDDIR)/glPix$(OSU) \
	$(LABLGL_BUILDDIR)/glTex$(OSU) \
	$(LABLGL_BUILDDIR)/glDraw$(OSU) \
	$(BUILDDIR)/config$(OSU) \
	$(LABLGL_BUILDDIR)/glArray$(OSU) \
	$(BUILDDIR)/main$(OSU) \
	$(BUILDDIR)/link.o

$(BUILDDIR)/llpp: lablGl mupdf $(LLPP_FILES)
	$(COMP) -g $(LFL) -I lablGl -o $@ unix$(ASU) str$(ASU) $(LLPP_FILES) -cclib '-lGl -lX11 -lmupdf -lmupdfthird -lpthread -Lmupdf/build/native -lcrypto $(LABLGL_BUILDDIR)/ml_gl.o $(LABLGL_BUILDDIR)/ml_glarray.o $(LABLGL_BUILDDIR)/ml_raw.o $(LIBGL_LFLAGS)'

mupdf:
	test -d mupdf || git clone git://git.ghostscript.com/mupdf --recursive && \
	cd mupdf && \
	make build=native XCFLAGS=$(LIBGL_CFLAGS) XLIBS=$(LIBGL_LFLAGS)

clean:
	rm -rf $(BUILDDIR)

full_clean: clean
	rm -rf mupdf

.PHONY: mupdf clean full_clean
