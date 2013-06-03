NAME = mrgsea
MANNAME = mrgsea.doc
OBJECTS = mrgsea.o ucase.o ljustn.o rjust.o match.o
# sun version 4.2 compiler can't use -O because of silent optimizer bug.
FFLAGS = -u -ANSI -C 
#FFLAGS = -u -ANSI -C -Bstatic 
# dec alpha
#FFLAGS = -u -C -stand semantic
prog: $(OBJECTS)
	g77 $(FFLAGS) $(OBJECTS) -o $(NAME)
clean:
	rm $(OBJECTS)
install:
	cp $(NAME) /usr/local/bin
	cp $(MANNAME) /usr/local/man
tar:
	tar cvf mrgsea.tar Makefile *.f mrgsea.doc
