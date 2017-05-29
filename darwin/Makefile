all : mainDarwin mainGen launchMain

mainDarwin :
	ocamlbuild -tag thread -lib unix main.native
	
mainGen :
	ocamlbuild -I types.ml -tag thread -lib unix genetique/mainGen.native
	
launchMain :
	ocamlbuild -tag thread -lib unix launchMain.native

clean :
	rm -rf _build/ genetique/_build/
	rm -f *.native genetique/*.native
	
	


