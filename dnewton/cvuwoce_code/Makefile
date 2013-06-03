NAME = cvuwoce
MANNAME = cvuwoce.doc
OBJECTS = cvuwoce.o newvol.o
FFLAGS = -O -u -ansi 
prog: $(OBJECTS)
	g77 $(FFLAGS) $(OBJECTS) -o $(NAME)
clean:
	rm $(OBJECTS)
install:
	cp $(NAME) /usr/local/bin
	cp $(MANNAME) /usr/local/man
tar:
	tar cvf cvuwoce.tar Makefile cvuwoce.doc *.f *.txt
