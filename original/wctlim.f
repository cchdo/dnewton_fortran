      subroutine wctlim(iob,le,bg,en,qualps,nast,nparam,
     *                  names,units,icd)
c
c -- routine to parse the 4th-6th header records that are in a WOCE .wct
c     (CTD data) format file.
c author: David Newton. Scripps Instn. of Oceanog. Apr94
c
      integer mxcnum
      parameter (mxcnum=9)
      integer iob,le,bg(-1:mxcnum),en(-1:mxcnum),
     *        qualps(-1:mxcnum),nast,nparam,icd
      character names(-1:mxcnum)*8,units(-1:mxcnum)*8
c
c le     ->  fortran error unit.
c iob    ->  fortran unit for .WCT file. already opened. will expect to read
c             4th line of file.
c bg     <- integer array filled with start positions of .WCT info.
c            indecies in data statement below.
c en     <- same as bg for ending column of info.
c qualps <- for parameters that have a quality code this array gives its
c           position in the left justified QUALT1 or QUALT2 code. If a
c           parameter doesn't have a quality byte then it is set to 0.
c nast   <- number of parameters that had asterisks under them
c nparam <- number of data values (recognized or not) on the data line.
c                  not including QUALTn.
c names  <- name  string for each parameter.
c units  <- units string for each parameter.
c icd    <- return code. 0=okay  1=some decoding error   non-zero=read error.
c
c   parameters that are not recorded will have bg(n)=0.

c
      integer mxextr,nextra,i,j,k,nfield,nfdone,lstart,mxline
      parameter (mxextr=10,mxline=130)
      character extra(mxextr)*8
      character line4*(mxline),line5*(mxline),line6*(mxline),
     *          string*8
      character numo(-1:mxcnum)*7
c
      intrinsic index,mod
c
c -- how to add more keywords to the "numo" list. ------------------------
c     a. add the mnemonic to the end (right justified upper case) of the array.
c     b. increase parameter "mxcnum"
c ------------------------------------------------------------------------
c
      data numo/
     * ' QUALT1',' QUALT2',
C         -1         0
     * ' CTDPRS',' CTDTMP',' CTDSAL',' CTDOXY','  XMISS',
C          1         2         3         4         5
     * '  FLUOR',' NUMBER','CTDCOND','  SOUND'/
C          6         7         8         9
c
c -- initialize some arrays.
      do 5 i=-1,mxcnum
         names(i) = ' '
         units(i) = ' '
         bg(i) = 0
         en(i) = 0
         qualps(i) = 0
 5    continue
c
c -- read the 3rd-5th header records.
      do 10 i=4,6
         if(i .eq. 4) read(iob,2010,iostat=icd) line4
         if(i .eq. 5) read(iob,2010,iostat=icd) line5
         if(i .eq. 6) read(iob,2010,iostat=icd) line6
 2010    format(a)
         if(icd .ne. 0) then
             write(le,*)'wctlim: read err in record #',i,
     *                  'iostat=',icd
             return
         endif
 10   continue
c
c -- parse the 4th line (mnemonic header).
c
c     -- look for QUALT1
      k = index(line4,'QUALT1')
      if(k .eq. 0) then
          write(le,*)'wctlim: no QUALT1 found on 2nd line.'
          icd = 1
          return
      endif
      en(-1) = k+5
c     -- look left for first character.
      do 20 i=k-1,1,-1
         if(line4(i:i) .ne. ' ') then
             bg(-1) = i+1
             go to 21
         endif
 20   continue
 21   continue 
c
c     -- look for possible QUALT2.
      k = index(line4,'QUALT2')
      if(k .eq. 0) then
c         -- no qualt2. no big deal
      else
          en(0) = k+5
          bg(0) = en(-1) + 1
      endif
c
      lstart = index(line4,'CTDPRS') - 2
      k = (bg(-1) - 1) - lstart + 1
c     -- k is now the number of columns of data. should be multiple of 8.
      if(mod(k,8) .ne. 0) then
c         -- data is not in neat 8 byte fields.
          write(le,*)'wctlim: data is not in neat 8 byte fields.'
          write(le,*)'        or CTDPRS not first on line.'
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
         string = line4(i:i+7)
         do 30 j=1,mxcnum
            if(index(string,numo(j)) .ne. 0) then
c               -- matched a header keyword (mnemonic). record position.
                bg(j) = i
                en(j) = i + 7
                names(j) = line4(bg(j):en(j))
c               -- grab units and check for quality asterisks.
                units(j) = line5(bg(j):en(j))
                if(line6(bg(j)+2:bg(j)+5) .eq. '****') then
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
             write(le,*)'wctlim: too many unrecognized keywords.'
             write(le,'(8(1x,a8))') (extra(j),j=1,mxextr)
             icd = 1
             return
         else
             extra(nextra) = string
         endif
         if(index(line6(i:i+7),'****') .ne. 0) then
             nast = nast + 1
         endif
 40   continue
c
      if(nfdone .ne. nfield) then
c         -- I programmed something wrong.
          write(le,*)'wctlim: prgrmer err. nfdone,nfield=',
     *                nfdone,nfield
          stop 99
      endif
      nparam = nfield
c
      if(nextra .gt. 0) then
c         -- print out unreccognized keywords.
          write(le,*)' these unrecognized keywords were found in',
     *               ' the .WCT file header.'
          write(le,'(8(1x,a8))') (extra(i),i=1,nextra)
      endif
c
c -- take possible hyphen out of temperature units.
      if(index(units(2),'68') .ne. 0) units(2) = 'IPTS68'
      if(index(units(2),'90') .ne. 0) units(2) = 'ITS90'
c
      return
      end
