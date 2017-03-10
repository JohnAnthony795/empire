BD=ocamlbuild -Xs Images -use-ocamlfind -lib lablgnomecanvas

TESTS=

all:
	${BD} cliempire.native ${TESTS}

clean:
	rm -rf _build cliempire.native
	find -L . -name "*~" -delete

