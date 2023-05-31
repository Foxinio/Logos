.PHONY: all compile clean test mod_student

all: build

build:
	dune build

install: all
	rm -f ./lgs
	ln -s _build/default/bin/main.exe ./lgs

clean:
	rm -f ./lgs
	dune clean
