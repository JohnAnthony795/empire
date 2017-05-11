all : mainDarwin mainGen

mainDarwin :
	ocamlbuild -use-ocamlfind main.native
	
mainGen :
	ocamlbuild -I types.mli genetique/mainGen.native
	
clean :
	rm -rf _build/ genetique/_build/
	rm *.native genetique/*.native
	
	


