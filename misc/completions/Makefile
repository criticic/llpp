# Create shell completions

COMPLETIONS = bash/llpp bash/llppac zsh/llpp zsh/llppac

# paths
PREFIX ?= /usr/local
datadir ?= ${PREFIX}/share

all: ${COMPLETIONS}

clean:
	${RM} ${COMPLETIONS}

install: all
	install -d ${DESTDIR}${datadir}/bash-completion/completions
	install -m644 bash/{llpp,llppac} \
		${DESTDIR}${datadir}/bash-completion/completions
	install -d ${DESTDIR}${datadir}/zsh/site-functions
	install -m644 zsh/llpp ${DESTDIR}${datadir}/zsh/site-functions/_llpp
	install -m644 zsh/llppac ${DESTDIR}${datadir}/zsh/site-functions/_llppac

%: mkcomp.sh llpp.in %.mk
	@sh mkcomp.sh $*

bash/llppac zsh/llppac: llppac.in

.PHONY: all clean install
