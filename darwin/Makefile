CURSES_LIB=-I,`ocamlfind query curses`
TARGETS:=main.ml
DIRECTORIES:=$(addprefix  -I , $(shell find sources -type d))
BIN_BYT:=$(patsubst %.ml, %.byte, $(TARGETS))
BIN_NAT:=$(patsubst %.ml, %.native, $(TARGETS))

SOURCES:=$(shell find sources -type f -name "*.ml")
INTF :=$(patsubst %.ml, %.inferred.mli, $(SOURCES))

OCAMLBUILD := ocamlbuild -cflag -g $(DIRECTORIES) -cflags -w,Ae,-warn-error,A -lflags -w,Ae,-warn-error,A -classic-display -use-menhir -menhir "menhir --explain --dump" -lib unix -lib curses -cflags ${CURSES_LIB} -lflags ${CURSES_LIB}

all: native

native:
	$(OCAMLBUILD) $(BIN_NAT)

byte:
	$(OCAMLBUILD) $(BIN_BYT)

intf:
	$(OCAMLBUILD) $(INTF)

clean:
	rm -rf _build
	rm -rf main.native
	rm -rf main.byte

dep:
	ocamldep -one-line $(DIRECTORIES) $(SOURCES) | \
	  grep -v "cmx" | \
	  python2.7 ./scripts/dot_of_depend.py | \
	  dot -Tsvg > dep.svg

stats:
	find sources -type f | xargs wc -l

test: all
	utop -I _build/sources
