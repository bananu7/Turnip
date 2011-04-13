PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

OPTS = -O3 #-fglasgow-exts -funbox-strict-fields
PACKAGES = -package parsec

SRC = Turnip.hs
PROG = Turnip

all:
	ghc $(PROF) $(OPTS) --make $(PACKAGES) $(SRC) -o $(PROG)

DIRS=.

install:
	install -c -m 755 $(PROG) $(BINDIR)
	
clean:
	for i in $(DIRS); do rm -f $$i/*.o $$i/*.hi; done

