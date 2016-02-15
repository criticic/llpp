BUILDDIR ?= build
CCOPT = $(CFLAGS) -Wno-pointer-sign -O2
MLOPT := -warn-error +a -w +a -g -safe-string
VERSION := $(or $(shell git describe --tags 2>/dev/null),unknown)
MLOPTGL := -I lablGL -I $(BUILDDIR)/lablGL
NATIVE =

ifeq ($(shell uname),Darwin)
LIBGL_LFLAGS := -L/opt/X11/lib
LIBGL_CFLAGS := -I/opt/X11/include
else
LIBGL_LFLAGS :=
LIBGL_CFLAGS :=
endif

ifdef NATIVE
COMP := ocamlopt.opt
OSU := .cmx
ASU := .cmxa
LFL :=
EXT := .native
else
COMP := ocamlc.opt
OSU := .cmo
ASU := .cma
LFL := -custom
EXT :=
endif

$(info VERSION = $(VERSION))
$(info BUILDDIR = $(BUILDDIR))
$(info COMP = $(COMP))
$(info LIBGL_LFLAGS = $(LIBGL_LFLAGS))
$(info LIBGL_CFLAGS = $(LIBGL_CFLAGS))

all: $(BUILDDIR)/llpp$(EXT)

LABLGL_FILES := \
	ml_raw.o \
	ml_gl.o \
	ml_glarray.o \
	gl$(OSU) \
	raw$(OSU) \
	glPix$(OSU) \
	glDraw$(OSU) \
	glTex.cmi \
	glMisc.cmi \
	glMat$(OSU) \
	glMisc$(OSU) \
	glFunc$(OSU) \
	glTex$(OSU) \
	glArray$(OSU) \
	glClear$(OSU)

LABLGL_FILES := $(addprefix lablGL/,$(LABLGL_FILES))

LABLGL_C_FILES := $(filter %.o,$(LABLGL_FILES))
LABLGL_MLI_FILES := $(filter %.cmi,$(LABLGL_FILES))
LABLGL_ML_FILES := $(filter %$(OSU),$(LABLGL_FILES))

$(addprefix $(BUILDDIR)/,$(LABLGL_C_FILES)): $(BUILDDIR)/%.o: %.c
	$(COMP) -ccopt '$(CCOPT) -o $@' -c $<

$(addprefix $(BUILDDIR)/,$(LABLGL_MLI_FILES)): $(BUILDDIR)/%.cmi: %.mli
	$(COMP) $(MLOPTGL) -o $@ -c $<

$(addprefix $(BUILDDIR)/,$(LABLGL_ML_FILES)): $(BUILDDIR)/%$(OSU): %.ml
	$(COMP) $(MLOPTGL) -o $@ -c $<

$(BUILDDIR)/lablGL:
	mkdir -p $@

lablGL: $(BUILDDIR)/lablGL $(addprefix $(BUILDDIR)/,$(LABLGL_FILES))

$(BUILDDIR)/link.o: link.c mupdf
	$(COMP) -ccopt '$(LIBGL_CFLAGS) -I mupdf/include -I mupdf/thirdparty/freetype/include -Wextra -Wall -Werror -D_GNU_SOURCE -O -g -std=c99 -pedantic-errors -Wunused-parameter -Wsign-compare -Wshadow -o $@' -c $<

$(BUILDDIR)/help.ml: KEYS mkhelp.sh
	./mkhelp.sh $< $(VERSION) > $@

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

LLPP_FILES := \
	help$(OSU) \
	utils$(OSU) \
	parser$(OSU) \
	wsi$(OSU) \
	config$(OSU) \
	main$(OSU) \
	link.o

LLPP_FILES := $(addprefix $(BUILDDIR)/,$(LLPP_FILES))
LABLGL_FILES := $(addprefix $(BUILDDIR)/,$(filter-out %.cmi,$(LABLGL_FILES)))

$(BUILDDIR)/llpp$(EXT): lablGL mupdf $(LLPP_FILES)
	$(COMP) -g $(LFL) -I lablGL -o $@ unix$(ASU) str$(ASU) $(LABLGL_FILES) $(LLPP_FILES) -cclib '-lGL -lX11 -lmupdf -lmupdfthird -lpthread -Lmupdf/build/native -lcrypto $(LIBGL_LFLAGS)'

mupdf:
	test -d mupdf || git clone git://git.ghostscript.com/mupdf --recursive && \
	cd mupdf && \
	make build=native XCFLAGS='$(LIBGL_CFLAGS)' XLIBS='$(LIBGL_LFLAGS)'

STDLIB = $(shell ocamlfind printconf stdlib)

test_osx.o: test_osx.ml
	$(COMP) -output-obj -o $@ $<

$(BUILDDIR)/main_osx: main_osx.m test_osx.o
	$(CC) -fmodules -fobjc-arc -I$(STDLIB) -L$(STDLIB) -lasmrun -o $@ $^

clean:
	rm -rf $(BUILDDIR)

full_clean: clean
	rm -rf mupdf

.PHONY: mupdf clean full_clean
