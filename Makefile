CLIENT := cd empire-client/; make all; cd ..;
SERVER := cd empire-server/; make all; cd ..;
GTK_CLIENT := cd empire-gtk-client/; make all; cd ..;
DARWIN := cd darwin/; make all; cd ..;

CLEAN_CLIENT := cd empire-client/; make clean; cd ..;
CLEAN_SERVER := cd empire-server/; make clean; cd ..;
CLEAN_GTK_CLIENT := cd empire-gtk-client/; make clean; cd ..;
CLEAN_DARWIN := cd darwin/; make clean; cd ..;



all :
	$(CLIENT) $(SERVER) $(GTK_CLIENT) $(DARWIN)
	
nous :
	$(SERVER) $(DARWIN)
	
clean:
	$(CLEAN_CLIENT) $(CLEAN_SERVER) $(CLEAN_GTK_CLIENT) $(CLEAN_DARWIN)
	
