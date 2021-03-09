.PHONY: clean build install check

IPKG=india


check:
	idris2 --typecheck ${IPKG}.ipkg

clean:
	idris2 --clean ${IPKG}.ipkg

build:
	idris2 --build ${IPKG}.ipkg

install: clean build
	idris2 --install ${IPKG}.ipkg

test: install
	replica tests
