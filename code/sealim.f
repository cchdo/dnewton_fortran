      subroutine sealim(iob,le,bg,en,qualps,nast,units,seaexp,icd)
c
c -- routine to parse the four header records that are in a WOCE .SEA
c     (hydro data) format file.
c author: David Newton. Scripps Instn. of Oceanog.
c
      integer mxnumo
      parameter (mxnumo=50)
      integer iob,le,bg(-1:mxnumo),en(-1:mxnumo),
     *        qualps(-1:mxnumo),nast,icd
      character units(-1:mxnumo)*8,seaexp*14
c
c le     ->  fortran error unit.
c iob    ->  fortran unit for .SEA file. already opened. will expect to read
c             first line of file.
c bg     <- integer array filled with start positions of .SEA info.
c            indecies in data statement below.
c en     <- same as bg for ending column of info.
c qualps <- for parameters that have a quality code this array gives its
c           position in the left justified QUALT1 or QUALT2 code. If a
c           parameter doesn't have a quality byte then it is set to 0.
c nast   <- number of parameters that had asterisks under them
c units  <- units string for each parameter.
c seaexp <- expocode from the 1st line of the .SEA file. 
c icd    <- return code. 0=okay  1=some decoding error   non-zero=read error.
c
c   parameters that are not recorded will have bg(n)=0.

c
      integer mxextr,nextra,i,j,k,l,nfield,nfdone,lstart,mxline
      parameter (mxextr=10,mxline=300)
      character extra(mxextr)*8
      character line1*(mxline),line2*(mxline),line3*(mxline),
     *          line4*(mxline),string*8
      character numo(-1:mxnumo)*7
c
      intrinsic index,mod
      external namget
c
c -- initialize numo array  (contains all parrmeter names)
      call namget('initial',numo,i)
      if(i .ne. mxnumo) then
          write(le,*)'mxnumo in sealim=',mxnumo,' mxnumo frm namget=',
     *                i
          write(le,*)'programmer error.'
          stop 99
      endif
c
c -- initialize some arrays.
      do 5 i=-1,mxnumo
         units(i) = ' '
         bg(i) = 0
         en(i) = 0
         qualps(i) = 0
 5    continue
c
c -- read the first 4 header records.
      do 10 i=1,4
         if(i .eq. 1) read(iob,2010,iostat=icd) line1
         if(i .eq. 2) read(iob,2010,iostat=icd) line2
         if(i .eq. 3) read(iob,2010,iostat=icd) line3
         if(i .eq. 4) read(iob,2010,iostat=icd) line4
 2010    format(a)
         if(icd .ne. 0) then
             write(le,*)'sealim: read err in first 4 recs.',
     *                  'iostat=',icd
             return
         endif
 10   continue
c
c -- extract the expocode from line 1.
      k = index(line1,'EXPO')
      if(k .eq. 0) then
          write(le,*)'sealim: no EXPO string in 1st header.'
          icd = 1
          return
      else
c         -- begin very silly do loops to extract expocode.
          do 16 i=k,mxline
             if(line1(i:i) .eq. ' ') then
                 do 15 j=i,mxline
                    if(line1(j:j) .ne. ' ') then
c                       -- j is start of expocode.
                        do 14 l=j,mxline
                           if(line1(l:l) .eq. ' ') then
c                              -- (l-1) is end of expocode.
                               seaexp = line1(j:l-1)
                               go to 19
                           endif
 14                     continue
                        go to 17
                    endif
 15              continue
                 go to 17
             endif
 16       continue
 17       continue
c         -- get here when can't extract the expocode.
          write(le,*)'sealim: can''t extract expocode from 1st hdr.'
          icd = 1
          return
      endif
c
c -- get to here with expocode extracted.
 19   continue
c
c -- parse the 2nd (mnemonic header).
c
c     -- look for QUALT1
      k = index(line2,'QUALT1')
      if(k .eq. 0) then
          write(le,*)'sealim: no QUALT1 found on 2nd line.'
          icd = 1
          return
      endif
      en(-1) = k+5
c     -- look left for first character.
      do 20 i=k-1,1,-1
         if(line2(i:i) .ne. ' ') then
             bg(-1) = i+1
             go to 21
         endif
 20   continue
 21   continue 
c
c     -- look for possible QUALT2.
      k = index(line2,'QUALT2')
      if(k .eq. 0) then
c         -- no qualt2. no big deal
      else
          en(0) = k+5
          bg(0) = en(-1) + 1
      endif
c
c -- now look for required STNNBR
      k = index(line2,'STNNBR')
      if(k .eq. 0) then
          write(le,*)'sealim: STNNBR string not found in 2nd header.'
          icd = 1
          return
      endif
c
      lstart = k - 2
      k = (bg(-1) - 1) - lstart + 1
c     -- k is now the number of columns of data. should be multiple of 8.
      if(mod(k,8) .ne. 0) then
c         -- data is not in neat 8 byte fields.
          write(le,*)'sealim: data is not in neat 8 byte fields.'
          icd = 1
          return
      endif
      nfield = k / 8
c
      nfdone = 0
      nextra = 0
      nast = 0
      do 40 i=lstart,bg(-1)-4,8
         nfdone = nfdone + 1
         string = line2(i:i+7)
         do 30 j=1,mxnumo
            if(index(string,numo(j)) .ne. 0) then
c               -- matched a header keyword (mnemonic). record position.
                bg(j) = i
                en(j) = i + 7
c               -- grab units and check for quality asterisks.
                units(j) = line3(bg(j):en(j))
                if(line4(bg(j)+2:bg(j)+5) .eq. '****') then
                    nast = nast + 1
                    qualps(j) = nast 
                endif
                go to 40
            endif
 30      continue
c
c        -- get to here if no keywords match.
         nextra = nextra + 1
         if(nextra .gt. mxextr) then
             write(le,*)'sealim: too many unrecognized keywords.'
             write(le,'(8(1x,a8))') (extra(j),j=1,mxextr)
             icd = 1
             return
         else
             extra(nextra) = string
         endif
         if(index(line4(i:i+7),'****') .ne. 0) then
             nast = nast + 1
         endif
 40   continue
c
      if(nfdone .ne. nfield) then
c         -- I programmed something wrong.
          write(le,*)'sealim: prgrmer err. nfdone,nfield=',
     *                nfdone,nfield
          stop 99
      endif
c
      if(nextra .gt. 0) then
c         -- print out unreccognized keywords.
          write(le,*)' these unrecognized keywords were found in',
     *               ' the .SEA file header.'
          write(le,'(8(1x,a8))') (extra(i),i=1,nextra)
      endif
c
      if(bg(1).eq.0 .or. bg(2).eq.0) then
c         -- missing STNNBR or CASTNO. need these to match .sum file.
          write(le,*)'sealim: missing STNNBR or CASTNO keywords.'
          icd = 1
          return
      endif
c
      return
      end
c ********************************************************************
      subroutine namget(what,namary,ipnt)
c
c -- routine to initialize the list of paramters that can be found
c     in the header of a .SEA file
c       or
c     return the number for the paramter name in the list.
c
      integer mxnumo
      parameter (mxnumo=50)
c
      integer ipnt
      character namary(-1:mxnumo)*7,what*7
c
      character numo(-1:mxnumo)*7,strng*7,strng2*7
      integer i,nsig 
c
      external ljustn
c
c -- how to add more keywords to the "numo" list. ------------------------
c   a. add the mnemonic to the end (right justified upper case) of the array.
c   b. increase parameter "mxnumo" in all routines that contain it.
c        including sealim,seaorder,seqqwk,seaun1,trndat,wocecvt,namget
c ------------------------------------------------------------------------
c
      data numo/
     * ' QUALT1',' QUALT2',
C         -1         0
     * ' STNNBR',' CASTNO',' SAMPNO',' BTLNBR',' CTDRAW',
C          1         2         3         4         5
     * ' CTDPRS',' CTDTMP',' CTDSAL',' CTDOXY','  THETA',
C          6         7         8         9        10
     * ' SALNTY',' OXYGEN',' SILCAT',' NITRAT',' NITRIT',
C         11        12        13        14        15
     * ' PHSPHT',' CFC-11',' CFC-12',' REVPRS',' REVTMP',
C         16        17        18        19        20
     * ' TRITUM',' HELIUM',' DELHE3',' DELC14',' DELC13',
C         21        22        23        24        25
     * '  KR-85','  ARGON','  AR-39','   NEON',' RA-228',
C         26        27        28        29        30
     * ' RA-226','O18/O16','  SR-90',' CS-137',' TCARBN',
C         31        32        33        34        35
     * ' ALKALI','   FCO2','     PH',' TRITER',' HELIER',
C         36        37        38        39        40
     * ' DELHER',' C-14ER',' C-13ER','KRP85ER',' ARGERR',
C         41        42        43        44        45
     * ' AR39ER',' NEONER',' R228ER',' R226ER','NO2+NO3'/
C         46        47        48        49        50
c
      ipnt = -2
      if(what.eq.'initial') then
          do 20 i=-1,mxnumo
             namary(i) = numo(i)
 20       continue
          ipnt = mxnumo
          return
      endif
c
      strng = what
      call ljustn(strng,nsig)
      if(strng(1:4) .eq. 'NONE') return
      do 30 i=-1,mxnumo
         strng2 = numo(i)
         call ljustn(strng2,nsig)
         if(strng .eq. strng2) then
c            -- have found perfect match.
             ipnt = i
             return
         endif
 30   continue
      ipnt = -3
      return
      end
