PREFIX=/usr/local
BIN=$(PREFIX)/bin

VERSION=1.1

DIST=upstream-$(VERSION)

all: clnet gna savannah sfnet

clnet gna savannh sfnet:
	sbcl --noinform --userinit /dev/null --sysinit /dev/null --disable-debugger \
             --load upstream.lisp \
             --eval "(progn (upstream:build-all \"$(VERSION)\") (quit))"
	chmod a+x clnet gna savannah sfnet

clean:
	rm -f clnet gna sfnet savannah

install: all
	install clnet $(BIN)
	install gna $(BIN)
	install savannah $(BIN)
	install sfnet $(BIN)

uninstall:
	rm $(BIN)/clnet
	rm $(BIN)/gna
	rm $(BIN)/savannah
	rm $(BIN)/sfnet

dist: all
	rm -rf $(DIST) $(DIST).tar.gz
	mkdir $(DIST)
	cp clnet gna savannah sfnet Makefile TODO.txt upstream.lisp $(DIST)
	tar -czvf $(DIST).tar.gz $(DIST)
	rm -rf $(DIST) 
