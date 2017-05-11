all : mainDarwin mainGen

mainDarwin :
	ocamlbuild -tag thread -lib unix main.native
	
mainGen :
	ocamlbuild -I types.ml -tag thread -lib unix genetique/mainGen.native
	
clean :
	rm -rf _build/ genetique/_build/
	rm *.native genetique/*.native
	
	


