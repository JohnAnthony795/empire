rm out_* &> /dev/null
rm empire-client/Main.* &> /dev/null
rm empire-server/Main.native &> /dev/null
rm empire-gtk-client/cliempire.native &> /dev/null
rm darwin/*.native darwin/*~ darwin/parsing/*~ &> /dev/null
rm -rf darwin/_build &> /dev/null
rm -rf _build &> /dev/null
rm -rf empire-gtk-client/_build &> /dev/null
cd empire-server; make clean; cd ..
