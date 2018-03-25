.PHONY: all
all: llpp.1 llppac.1 llpphtml.1

%.1: %.xml
	xmlto man -o . $<

%.xml: %.man asciidoc.conf
	asciidoc -d manpage -b docbook -f asciidoc.conf -o $@ $<

.PHONY: clean
clean:
	rm -f llpp.1 llppac.1 llpphtml.1
