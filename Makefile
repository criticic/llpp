OCAMLBUILD = ocamlbuild -classic-display
LN = ln
RM = rm
LLPP = llpp
VERSION = $(or $(shell git describe --tags 2>/dev/null),unknown)

native:
	VERSION=$(VERSION) $(OCAMLBUILD) main.native && \
	$(LN) -f _build/main.native $(LLPP).native

byte:
	VERSION=$(VERSION) $(OCAMLBUILD) main.byte && \
	$(LN) -f _build/main.byte $(LLPP).byte

mupdf:
	test -d mupdf || git clone git://git.ghostscript.com/mupdf --recursive && \
	cd mupdf && \
	make build=native XCFLAGS='-I /opt/X11/include' XLIBS='-L /opt/X11/lib'

clean:
	$(OCAMLBUILD) -clean
	$(RM) -f $(LLPP).native $(LLPP)

.PHONY: all clean mupdf

# $(BUILDDIR)/llpp.o: lablGL mupdf $(LLPP_FILES)
# 	$(COMP) -verbose -g $(LFL) -output-obj -I lablGL -o $@ -ccopt '-o $@' unix$(ASU) str$(ASU) $(LABLGL_FILES) $(LLPP_FILES)

# STDLIB = $(shell ocamlfind printconf stdlib)

# ifdef NATIVE
# LIBCAML := asmrun
# else
# LIBCAML := camlrun
# endif

# $(BUILDDIR)/llpp_osx.o: main_osx.m
# 	$(CC) -fmodules -fobjc-arc -I$(STDLIB) $(LIBGL_CFLAGS) -o $@ -c $<

# $(BUILDDIR)/llpp_osx: $(BUILDDIR)/llpp_osx.o $(BUILDDIR)/llpp.o
# 	$(CC) -L$(STDLIB) -Lmupdf/build/native $(LIBGL_LFLAGS) -lGL -lX11 -lmupdf -lmupdfthird -lunix -lcamlstr -ltermcap -l$(LIBCAML) -framework cocoa $(LABLGL_C_FILES) $(BUILDDIR)/link.o $^ -o $@
