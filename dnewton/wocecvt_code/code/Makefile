NAME = wocecvt
NAME2 = sumchk
NAME3 = seaorder
NAME4 = wctcvt
MANNAME = wocecvt.doc sumchk.doc seaorder.doc wctcvt.doc woce_sum.notes
OBJECTS = wocecvt.o sumlim.o sealim.o sumqwk.o seaqwk.o sumun1.o seaun1.o \
          fndsum.o cdieh.o trndat.o dodept.o rtnfmt.o ljustn.o pzcon.o \
          cdsd2.o dounit.o chkdep.o prtieh.o rgtieh.o prtsd2.o wrtsd2.o \
          cartertab.o newvol.o
SRC1 = wocecvt.f sumlim.f sealim.f sumqwk.f seaqwk.f sumun1.f seaun1.f \
          fndsum.f cdieh.f trndat.f dodept.f rtnfmt.f ljustn.f pzcon.f \
          cdsd2.f dounit.f chkdep.f prtieh.f rgtieh.f prtsd2.f wrtsd2.f \
          cartertab.f newvol.f
OBJECTS2 = sumchk.o sumlim.o sumqwk.o sumun1.o ljustn.o rtnfmt.o
SRC2 = sumchk.f sumlim.f sumqwk.f sumun1.f ljustn.f rtnfmt.f
OBJECTS3 = seaorder.o sealim.o seaqwk.o ljustn.o lmsort.o
SRC3 = seaorder.f sealim.f seaqwk.f ljustn.f lmsort.f 
OBJECTS4 = wctcvt.o sumlim.o sumun1.o sumqwk.o ljustn.o rtnfmt.o fndsum.o\
            cartertab.o wctun1.o wctlim.o wctun2.o cddcfh.o cddcfu.o\
            cddcfd.o
SRC4 = wctcvt.f sumlim.f sumun1.f sumqwk.f ljustn.f rtnfmt.f fndsum.f\
            cartertab.f wctun1.f wctlim.f wctun2.f cddcfh.f cddcfu.f\
            cddcfd.f
FFLAGS = -O
wocecvt: $(OBJECTS)
	g77 $(FFLAGS) $(OBJECTS) -o $(NAME)
sumchk: $(OBJECTS2)
	g77 $(FFLAGS) $(OBJECTS2) -o $(NAME2)
seaorder: $(OBJECTS3)
	g77 $(FFLAGS) $(OBJECTS3) -o $(NAME3)
wctcvt: $(OBJECTS4)
	g77 $(FFLAGS) $(OBJECTS4) -o $(NAME4)
clean:
	rm -f *.o *.tar
all: wocecvt sumchk seaorder wctcvt
tar:
	tar cvf wocecvt.tar Makefile README  *.doc *.f *.notes *.all
zip:
	zip -g wocecvt.zip Makefile README  *.doc *.f *.notes *.all
concat:
	rm -f wocecvt.all
	cat $(SRC1) > wocecvt.all
	rm -f sumchk.all
	cat $(SRC2) > sumchk.all
	rm -f seaorder.all
	cat $(SRC3) > seaorder.all
	rm -f wctcvt.all
	cat $(SRC4) > wctcvt.all
install:
	cp $(NAME) /usr/local/bin
	cp $(NAME2) /usr/local/bin
	cp $(NAME3) /usr/local/bin
	cp $(NAME4) /usr/local/bin
	cp $(MANNAME) /usr/local/man
