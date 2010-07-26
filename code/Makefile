COMPILE = gfortran
TO_OBJ = -c
#ALL_FILES = rtnfmt.f ljustn.f sumlim.f sumqwk.f sumun1.f sumchk.f
#ALL_OBJS = rtnfmt.o ljustn.o sumlim.o sumqwk.o sumun1.o sumchk.o
#APP_NAME = sumchk

ALL_FILES = wocecvt.f sumlim.f sealim.f sumqwk.f seaqwk.f sumun1.f \
            seaun1.f fndsum.f cdieh.f trndat.f dodept.f rtnfmt.f \
            ljustn.f pzcon.f cdsd2.f dounit.f chkdep.f prtieh.f \
            rgtieh.f prtsd2.f wrtsd2.f cartertab.f newvol.f
ALL_OBJS = wocecvt.o sumlim.o sealim.o sumqwk.o seaqwk.o sumun1.o \
          seaun1.o \
          fndsum.o cdieh.o trndat.o dodept.o rtnfmt.o ljustn.o pzcon.o \
          cdsd2.o dounit.o chkdep.o prtieh.o rgtieh.o prtsd2.o wrtsd2.o \
          cartertab.o newvol.o
APP_NAME = asdf

all:
	$(COMPILE) $(TO_OBJ) $(ALL_FILES)
	$(COMPILE) $(ALL_OBJS) -o $(APP_NAME)
clean:
	rm *.o asdf
