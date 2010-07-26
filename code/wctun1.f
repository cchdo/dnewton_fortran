      subroutine wctun1(iob,le,cexpo,cwhpid,cyear,cmonth,cday,stnnbr,
     *                  castno,nobs,instru,samphz,icd)
c
c -- routine to read and uncode the first 3 lines of a WOCE format high
c    resolution CTD data file. *.wct.
c -- David Newton, SIO apr94.
c
      integer iob,le,cyear,cmonth,cday,castno,nobs,icd
      real samphz
      character cexpo*14,cwhpid*5,stnnbr*6,instru*4
c
c   iob    ->  fortran unit for opened *.wct file. ready to read 1st line.
c   le     ->  fortran error unit.
c   cexpo  <-  14 char left justified expo code from the data file.
c   cwhpid <-  5 char left justified WOCE line (section ) id.
c   cyear  <-  2 digit year from data file.
c   cmonth <-  2 digit month from data file.
c   cday   <-  2 digit day from data file.
c   stnnbr <-  6 character station id from data file.
c   castno <-  castnumber (integer) from data file.
c   nobs   <-  number of high res levels in cast.
c   instru <-  4 character instrument id from data file.
c   samphz <-  sampling rate in hz. real.
c   icd    <-  error return code.
c              0==okay, 1==warning, 2==fatal err, negative=end_of_file
c
      character line*60,scrtch*20,fmt*20
      integer i,j,k,nsig
      logical warn
c
      intrinsic index
      external ljustn
c
      warn = .false.
c
c
c -- uncode the 1st header line.
c
      read(iob,2010,iostat=icd) line
 2010  format(a)
      if(icd .ne. 0) then
          write(le,*)'wctun1: read err on 1st line. iostat=',icd
          go to 300
      endif
c
      i = index(line,'EXPOCODE')
      if(i .eq. 0) then
          write(le,*)'wctun1: no EXPOCODE word found. line=',line
          go to 350
      endif
      j = index(line,'WHP')
      if(j .eq. 0) then
          write(le,*)'wctun1: no WHP-ID word found. line=',line
          go to 350
      endif
      if(i+8 .gt. j-1) then
          write(le,*)'wctun1: 1st header line screwed up. line=',line
          go to 350
      endif
      scrtch = line(i+8:j+1)
      call ljustn(scrtch,nsig)
      k = 14
      do 20 i=2,14
         if(scrtch(i:i) .eq. ' ') then
             k = i-1
             go to 21
         endif
 20   continue
 21   continue
      cexpo = scrtch(1:k)
c
      i = j
      j = index(line,'DATE')
      if(j .eq. 0) then
          write(le,*)'wctun1: no DATE word found. line=',line
          go to 350
      endif
      if(i+6 .gt. j-1) then
          write(le,*)'wctun1: 1st header line screwed up. line=',line
          go to 350
      endif
      scrtch = line(i+6:j-1)
      call ljustn(scrtch,nsig)
      cwhpid = scrtch
c
      scrtch = line(j+4:j+14)
      call ljustn(scrtch,nsig)
      if(scrtch(6:6) .eq. ' ') then
c         -- 5 char date.
          read(scrtch,'(i1,2i2)',iostat=icd) cmonth,cday,cyear
      else
c         -- 6 char date. (as it should be).
          read(scrtch,'(3i2)',iostat=icd) cmonth,cday,cyear
      endif
      if(icd .ne. 0 ) then
          write(le,*)'wctun1: read err on date. iostat,line=',icd,line
          warn = .true.
          cmonth = -9
          cday = -9
          cyear = -9
      endif
c
c -- uncode the second header line.
c
      read(iob,2010,iostat=icd) line
      if(icd .ne. 0) then
          write(le,*)'wctun1: read err on 2nd line. iostat=',icd
          go to 300
      endif
c
      i = index(line,'STNNBR')
      if(i .eq. 0) then
          write(le,*)'wctun1: no STNNBR word found. line=',line
          go to 350
      endif
      j = index(line,'CASTNO')
      if(j .eq. 0) then
          write(le,*)'wctun1: no CASTNO word found. line=',line
          go to 350
      endif
      if(i+6 .gt. j-1) then
          write(le,*)'wctun1: 2nd header line screwed up. line=',line
          go to 350
      endif
      scrtch = line(i+6:j-1)
      call ljustn(scrtch,nsig)
      stnnbr = scrtch
c
      i = j
      j = index(line,'NO.')
      if(j .eq. 0) then
          write(le,*)'wctun1: no NO. RECORDS= word found. line=',line
          go to 350
      endif
      k = (j-1) - (i+6) + 1
      if(k .lt. 1) then
          write(le,*)'wctun1: 2nd header line screwed up. line=',line
          go to 350
      endif
      write(fmt,2020,iostat=icd) k
 2020  format('(I',i2,')')
      if(icd .ne. 0) then
          write(le,*)'wctun1: err forming format for castno read.',
     *               'notify programmer. icd=',icd
          fmt = '(i4)'
      endif
      read(line(i+6:j-1),fmt,iostat=icd) castno
      if(icd .ne. 0) then
          write(le,*)'wctun1: read err on CASTNO. iostat,line=',
     *                icd,line
          go to 300
      endif
c
      i = index(line,'ORDS=')
      if(i .eq. 0) then
          write(le,*)'wctun1: no NO. RECORDS= word found. line=',line
          go to 350
      endif
      scrtch = line(i+5:i+5+10)
      call ljustn(scrtch,nsig)
      do 50 i=2,nsig+1
         if(scrtch(i:i) .eq. ' ') then
             k = i-1
             go to 51
         endif
 50   continue
      write(le,*)'wctun1: NO. RECORDS= unreadable. line=',line
      go to 350
 51   continue
      write(fmt,2020,iostat=icd) k
      if(icd .ne. 0) then
          write(le,*)'wctun1: err forming format for RECORDS= read.',
     *               'notify programmer. icd=',icd
          fmt = '(i5)'
      endif
      read(scrtch(1:k),fmt,iostat=icd) nobs
      if(icd .ne. 0) then
          write(le,*)'wctun1: read err on NO. RECORDS. iostat,line=',
     *                icd,line
          write(le,*)'       fmt=',fmt
          go to 300
      endif
c
c -- uncode 3rd header.
c
      read(iob,2010,iostat=icd) line
      if(icd .ne. 0) then
          write(le,*)'wctun1: read err on 2nd line. iostat=',icd
          go to 300
      endif
c
      i = index(line,'NO.')
      if(i .eq. 0) then
          write(le,*)'wctun1: no INSTRUMENT NO. word found. line=',
     *                line
          instru = '-'
          warn = .true.
          go to 210
      endif
      j =  index(line,'SAMP')
      if(j .eq. 0) then
          write(le,*)'wctun1: no SAMPLING RATE word found. line=',line
          instru = '-'
          warn = .true.
          go to 210
      endif
      if(i+3 .gt. j-1) then
          write(le,*)'wctun1: 3rd header line screwed up. line=',line
          instru = '-'
          warn = .true.
          go to 210
      endif
      scrtch = line(i+3:j-1)
      call ljustn(scrtch,nsig)
      instru = scrtch
      if(nsig .eq. 0) instru = '-'
c
 210  continue
      i = index(line,'RATE')
      if(i .eq. 0) then
          write(le,*)'wctun1: no SAMPLING RATE word found. line=',line
          samphz = -9.
          warn = .true.
          go to 220
      endif
      j = index(line,'HZ')
      if(j .eq. 0) then
          write(le,*)'wctun1: no HZ word found. line=',line
          samphz = -9.
          warn = .true.
          go to 220
      endif
      k = (j-1) - (i+4) + 1
      if(k .lt. 2) then
          write(le,*)'wctun1: can''t read sampling rate.'
          samphz = -9.
          warn = .true.
          go to 220
      endif
      write(fmt,2040,iostat=icd) k
 2040  format('(F',i2,'.2)')
      if(icd .ne. 0) then
          write(le,*)'wctun1: err forming format for hz. read.',
     *               'notify programmer. icd=',icd
          fmt = '(f7.2)'
      endif
      read(line(i+4:j-1),fmt,iostat=icd) samphz
      if(icd .ne. 0) then
          write(le,*)'wctun1: read err on HZ. iostat,line=',
     *                icd,line
          samphz = -9.
          warn = .true.
          go to 220
      endif
c
c -- get to here on everything normal and warnings only.
 220  continue
      if(warn) icd = 1
      return
c
c -- get to here on read error of some fatal type.
 300  if(icd .gt. 0) icd = 2
      return
c
c -- get to here on some fatal parsing error.
 350  icd = 2
      return
c
      end
  
