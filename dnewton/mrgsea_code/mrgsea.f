c program nrgsea merges data into an existing WOCE bottle format file "sea"
c   file.
c
      integer li,lo,ioa,iob,icd
      integer maxnew,mxll,mxdc
      parameter (maxnew=17000,mxll=400,mxdc=39)
      integer i,j,ii,k,nlts,naqf,mmode,fqf(2),sas(2),acastn(maxnew),
     *        qf(maxnew,2),nndl,ncinfi,ctm,q1,q2,eol,nast,ldc,qx(mxdc),
     *        nef,nnast,neol,iqf1,iqf2,widqf,rb1,re1,ilnum,idp,
     *        kc1,kc2,kc3,kc4,kc5,ecastn,tqf1,tqf2
      integer istn(maxnew),ibotnm(maxnew),isamp(maxnew)
      real a,val(maxnew),apress(maxnew),epress,prslim,misval
      logical add,qfph,xist,used(maxnew)
c
      character name*40,line*(mxll),rtf*80,c8*8,c1*1,c10*10,
     *          astn(maxnew)*8,asamp(maxnew)*8,abotnm(maxnew)*8,
     *          hline(4)*(mxll),colnam(mxdc)*8,coluni(mxdc)*8,
     *          colast(mxdc)*8,newnam*8,newuni*8,oname*40,c40*40,
     *          d40*40,ofmt*6
      character estn*8,esamp*8,ebotnm*8
c     
      intrinsic nint,lge,lle,len,mod,index
      external ucase,ljustn,rjust,nschar,match
      data li/5/,lo/6/,ioa/12/,iob/14/
      data kc1/-1/,kc2/-1/,kc3/-1/,kc4/-1/,kc5/-1/
c
c changes:
c  06Mar01 dmn. fixed lame code for detecting non-unique parameter name.
c
      write(lo,*)'program mrgsea merges new data into an existing'
      write(lo,*)' WOCE format bottle .sea or .hyd file.'
      write(lo,*)'Only one data column will be merged each time.'
c
 20   write(lo,*)'enter filename of the new data file:'
      read(li,2010) name
 2010  format(a)
      open(ioa,file=name,status='old',iostat=icd)
      if(icd .ne. 0) then
          write(lo,*)'can''t open ',name,' iostat=',icd,' try again.'
          go to 20
      endif
c
      do 30 i=1,5
         read(ioa,2010,iostat=icd) line
         if(icd .ne. 0) then
             write(lo,*)'read err trying line#',i
             go to 30
         endif
         call nschar(line,j)
         if(j .eq. 0) j = 1
         write(lo,2010) line(1:j)
 30   continue
c
c - number of lines to skip.
 40   write(lo,*)'lines above are from the new file.'
      write(lo,*)'enter number of lines to skip before ''real'' data:'
      read(li,*,iostat=icd) a
      if(icd .ne. 0) then
          write(lo,*)'read error on number of line to skip.'
          write(lo,*)' expecting an integer. try again.'
          go to 40
      endif
      nlts = nint(a)
c
c -- bottle matching mode
 50   write(lo,*)'1. STNNBR, CASTNO, SAMPNO'
      write(lo,*)'2. STNNBR, CASTNO, BTLNBR'
      write(lo,*)'3. STNNBR, SAMPNO'
      write(lo,*)'4. STNNBR, CASTNO, CTDPRS'
      write(lo,*)'5. STNNBR, CTDPRS'
      write(lo,*)' Bottles must be matched. From list above choose',
     *           ' a method.'
      write(lo,*)' number 1 is much preferred (1,2,3,4,5):'  
      read(li,*,iostat=icd) a
      if(icd .ne. 0) then
          write(lo,*)'read error on match mode.'
          write(lo,*)' expecting an integer. try again.'
          go to 50
      endif
      mmode = nint(a)
      if(mmode .lt. 1 .or. mmode .gt. 5) then
          write(lo,*)'must enter an integer 1-5. try again.'
          go to 50
      endif
      if(mmode.eq.4 .or. mmode.eq.5) then
c         -- ask for pressure closeness limit.
 56       write(lo,*)'how many decibars is close enough for a',
     *               ' pressure match?:'
          read(li,*,iostat=icd) prslim
          if(icd .ne. 0) then
              write(lo,*)'read error on pressure closeness. enter',
     *                   ' a real number:'
              go to 56
          endif
      endif
c
c -- number of attached quality flags.
 60   write(lo,*)'How many quality flags are associated with the one',
     *           'data column you want to add? (0,1,2):'
      read(li,*,iostat=icd) a
      if(icd .ne. 0) then
          write(lo,*)'read error on number of quality flags.'
          write(lo,*)' expecting an integer 0,1,2 . try again.'
          go to 60
      endif
      naqf = nint(a)
      if(naqf .lt. 0 .or. naqf .gt. 2) then
          write(lo,*)'must enter an integer 0,1,2 . try again.'
          go to 60
      endif
c
c -- specifying fortran run-time format
c
      write(lo,*)'next you''ll need to specify a valid fortran runtime',
     *           ' format to read the data.'
      write(lo,*)'your format will require these elements:'
      if(naqf .eq. 0) then
          c8 = ' '
      else if(naqf .eq. 1) then
          c8 = ', i )'
      else if(naqf .eq. 2) then
          c8 = ', i, i )'
      else
          write(lo,*)'main: dropped through naqf tests setting c8'
          write(lo,*)'programmer error.'
          stop 99
      endif
      if(mmode .eq. 1 .or.mmode .eq. 2) then
          write(lo,*)'(a ,i ,a ,f' // c8
      else if(mmode .eq. 3) then
          write(lo,*)'(a ,a ,f' // c8
      else if(mmode .eq. 4) then
          write(lo,*)'(a ,i ,f ,f' // c8
      else if(mmode .eq. 5) then
          write(lo,*)'(a ,f ,f' // c8
      else
          write(lo,*)'main: dropped through mmode tests in format sect.'
          write(lo,*)'programmer error.'
          stop 99
      endif
c
 68   write(lo,*)'enter runtime format:'
      read(li,2010) rtf
c -- find last non-blank character.
      call nschar(rtf,ii)
      if(ii .le. 0) then
          write(lo,*)'run time format is blank? try again.'
          go to 68
      endif
c
c -- check for parens at begin and end.
 70   if(rtf(1:1).eq.'(' .and. rtf(ii:ii).eq.')') then
c         -- beginning and end look good.
      else
          if(ii+2 .gt. len(rtf)) then
              write(lo,*)'runtime format too long. max=',len(rtf)
              go to 68
          endif
          if(rtf(ii:ii) .ne. ')') then
              ii = ii + 1
              rtf(ii:ii) = ')' 
          else if(rtf(1:1) .ne. '(') then
              do 74 i=ii,1,-1
                 rtf(i+1:i+1) = rtf(i:i)
 74           continue
              rtf(1:1) = '('
          endif
          go to 70 
      endif
c -- look for some typos I make a lot.
      if(index(rtf,'.f').gt.0 .or. index(rtf,'.i').gt.0 .or.
     *   index(rtf,'.a').gt.0 .or. index(rtf,'.t').gt.0 .or.
     *   index(rtf,'.x').gt.0 ) then
          write(lo,*)'typo?  . in place of , ?'
          go to 68
      endif
c -- find size of 'a' strings  
      k = 0
      do 76 i=1,ii
         if(rtf(i:i).eq.'a' .or. rtf(i:i).eq.'A') then
             c1 = rtf(i+1:i+1)
             k = k + 1
             if(lge(c1,'0') .and. lle(c1,'9')) then
                 read(c1,'(i1)') sas(k)
                 if(sas(k) .gt. 8) then
                     write(lo,*)'max size for ''a'' strings is 8'
                     go to 68
                 endif
                 if(rtf(i+2:i+2) .ne. ',') then
                     write(lo,*)'was expecting a comma after ',
     *                          rtf(i:i+1)
                     go to 68
                 endif
             else
                 write(lo,*)'was expecting an integer after ''a''',
     *                      ' found a:',c1
                 go to 68
             endif
             if(k.ge.2) go to 77
         endif
 76   continue
 77   continue
c
c -- test runtime format against fake data.
      write(lo,*)'here''s the first line of new data followed by',
     *           ' fake data using your format.'
      rewind(ioa)
      do 80 i=1,nlts+1
         read(ioa,2010) line
 80   continue
      call nschar(line,j)
      if(j.eq.0) j = 1
      write(lo,2010) line(1:j)
c
      c10 = '123456789A'
      fqf(1) = 1
      fqf(2) = 2
      if(mmode.eq.1 .or. mmode.eq.2) then
          write(lo,rtf,iostat=icd)c10(1:sas(1)),1,c10(1:sas(2)),
     *                            12.345,(fqf(i),i=1,naqf)
      else if(mmode .eq. 3) then
          write(lo,rtf,iostat=icd)c10(1:sas(1)),c10(1:sas(2)),
     *                            12.345,(fqf(i),i=1,naqf)
      else if(mmode .eq. 4) then
          write(lo,rtf,iostat=icd)c10(1:sas(1)),1,12.3,
     *                            12.345,(fqf(i),i=1,naqf)
      else if(mmode .eq. 5) then
          write(lo,rtf,iostat=icd)c10(1:sas(1)),12.3,
     *                            12.345,(fqf(i),i=1,naqf)
      else
          write(lo,*)'main:dropped through rtf output test.'
          write(lo,*)'programmer error.'
          stop 99
      endif
      if(icd .ne. 0) then
          write(lo,*)'error trying to test your format by using',
     *               ' it to output fake data.'
          write(lo,*)'try again.'
          go to 68
      endif
c
 86   write(lo,*)'does that look okay? (y/n):'
      read(li,2010) c1
      if(c1 .eq. 'y' .or. c1 .eq. 'Y') then
      else if(c1 .eq. 'n' .or. c1 .eq. 'N') then
          go to 68
      else
          write(lo,*)'please answer y or n.'
          go to 86
      endif
c
c -- read whole input file.
c
      rewind(ioa)
      do 90 i=1,nlts
         read(ioa,2010) line
 90   continue
c
      k = 0
 100  continue
      k = k + 1
      if(k .gt. maxnew) then
          write(lo,*)'new data file has too many lines. max=',maxnew
          stop 99
      endif
      if(mmode .eq. 1) then
          read(ioa,rtf,iostat=icd) astn(k),acastn(k),asamp(k),
     *                             val(k),(qf(k,j),j=1,naqf)
      else if(mmode .eq. 2) then
          read(ioa,rtf,iostat=icd) astn(k),acastn(k),abotnm(k),
     *                             val(k),(qf(k,j),j=1,naqf)
      else if(mmode .eq. 3) then
          read(ioa,rtf,iostat=icd) astn(k),asamp(k),
     *                             val(k),(qf(k,j),j=1,naqf)
      else if(mmode .eq. 4) then
          read(ioa,rtf,iostat=icd) astn(k),acastn(k),apress(k),
     *                             val(k),(qf(k,j),j=1,naqf)
      else if(mmode .eq. 5) then
          read(ioa,rtf,iostat=icd) astn(k),apress(k),
     *                             val(k),(qf(k,j),j=1,naqf)
      else
          write(lo,*)'prgrmr err. dropped through ifblock aft st100'
          stop 99
      endif
      if(icd .ne. 0) then
          if(icd .lt. 0) then
c             -- normal eof.
              close(ioa)
              nndl = k - 1
              write(lo,*)nndl,' data lines read.'
              go to 130
          endif
110       if(k .eq. 1) then
              write(lo,*)'read error on first line of data.'
              write(lo,*)'Retry or Abort? (r/a):'
              read(li,2010) c1
              if(c1 .eq. 'r' .or. c1 .eq. 'R') then
                   go to 68
              else if(c1 .eq. 'a' .or. c1 .eq. 'A') then
                   stop 99
              else
                   write(lo,*)'please enter  r or a:'
                   go to 110
              endif
          endif
          write(lo,*)'read error on new file. line number=',
     *                k+nlts,' iostat=',icd
          write(lo,*)'runtime format was: ',rtf
          stop 99
      endif
c     -- test if qualt flags are in range 1-9.
      do 112 j=1,naqf
         if(qf(k,j).lt.1 .or. qf(k,j).gt.9) then
c            -- invalid quality flag.
             write(lo,*)'invalid quality flag=',qf(k,j),' on line',
     *                  'number ',k+nlts
             stop 99
         endif
 112  continue 
      go to 100
c
c -- get here with whole new data file read.
c
 130  continue
c -- see if STNNBR,SAMPNO,BTLNBR can be converted to integers.
      do 140 i=1,nndl
         read(astn(i),'(bn,i8)',iostat=icd) istn(i)
         if(icd .ne. 0) istn(i) = -9999
         call ljustn(astn(i),j)
         if(mmode.eq.1 .or. mmode.eq.3) then
             read(asamp(i),'(bn,i8)',iostat=icd) isamp(i)
             if(icd .ne. 0) isamp(i) = -9999
             call ljustn(asamp(i),j)
         endif
         if(mmode.eq.2) then
             read(abotnm(i),'(bn,i8)',iostat=icd) ibotnm(i)
             if(icd .ne. 0) ibotnm(i) = -9999
             call ljustn(abotnm(i),j)
         endif
         used(i) = .false.
 140  continue
c    
c -- start dealing with the existing .sea file.
 160  write(lo,*)'enter existing .sea or .hyd filename:'
      read(li,2010) name
      open(ioa,file=name,status='old',iostat=icd)
      if(icd .ne. 0) then
          write(lo,*)'can''t open ',name,'iostat=',icd
          write(lo,*)' try again.'
          go to 160
      endif
c
c -- read the 4 header lines.
      do 170 i=1,4
         read(ioa,2010,iostat=icd) hline(i)
         if(icd .ne. 0) then
             write(lo,*)'read error in first 4 lines of:',name
             stop 99
         endif
 170  continue
c -- spit out expo code.
      write(lo,2010) hline(1)(1:50)
c 
c -- look at the second line and look for qualt2 then qualt1.
      q2 = index(hline(2),'QUALT2')
      q1 = index(hline(2),'QUALT1')
      if(q2 .gt. 0) then
c         -- qualt2 found.
          write(lo,*)' QUALT2 detected.'
          if(q1 .le. 0) then
              write(lo,*)'found QUALT2, but no QUALT1 ?????'
              stop 99
          else if(q1 .ge. q2) then
              write(lo,*)'QUALT2 comes before QUALT1 ?????'      
              stop 99
          endif
          eol = q2 + 5
          nef = 2
      else
c         -- qualt2 not found.
          write(lo,*)' no QUALT2 found.'
          if(q1 .le. 0) then
              write(lo,*)'main: no QUALT1 or QUALT2 found. Not WOCE',
     *                   ' Format. Aborting.'
              stop 99
          endif
          eol = q1 + 5
          nef = 1
      endif
c
c -- find last data column
      do 175 i=q1-1,1,-1
         if(hline(2)(i:i) .ne. ' ') then
c            -- have found end of last data column.
             ldc = i
             go to 177
         endif
 175  continue
      write(lo,*)'main: dropped out of 175lp. something''s wrong ',
     *            'with headers.'
      stop 99
c
 177  continue
      if(mod(ldc,8) .ne. 0) then
          write(lo,*)'main: last data column not on 8 byte boundary.'
          write(lo,*)'      aborting.'
          stop 99
      endif
c
      ncinfi = ldc / 8
      if(ncinfi .gt. mxdc) then
          write(lo,*)'too many data columns. max=',mxdc
          stop 99
      endif
c
c -- find beginning positions of columns likely to be read.
      i = index(hline(2),'STNNBR')
      if(i .gt. 0) kc1 = i - 2
      i = index(hline(2),'CASTNO')
      if(i .gt. 0) kc2 = i - 2
      i = index(hline(2),'SAMPNO')
      if(i .gt. 0) kc3 = i - 2
      i = index(hline(2),'BTLNBR')
      if(i .gt. 0) kc4 = i - 2
      i = index(hline(2),'CTDPRS')
      if(i .gt. 0) kc5 = i - 2
c
c -- check to see if these kcX positions are enough for the matchmode well
c    be using.
      if(kc1 .le. 0) then
          write(lo,*)'file:',name,' does not have STNNBR. aborting'
          stop 99
      endif
      if(mmode.eq.1) then
          if(kc2.le.0 .or. kc3.le.0) then
              write(lo,*)'file:',name,' missing CASTNO or SAMPNO.'
              stop 99
          endif
      else if(mmode .eq. 2) then
          if(kc2.le.0 .or. kc4.le.0) then
              write(lo,*)'file:',name,' missing CASTNO or BTLNBR.'
              stop 99
          endif
      else if(mmode .eq. 3) then
          if(kc3 .le. 0) then
              write(lo,*)'file:',name,' missing SAMPNO.'
              stop 99
          endif
      else if(mmode .eq. 4) then
          if(kc2.le.0 .or. kc5.le.0) then
              write(lo,*)'file:',name,' missing CASTNO or CTDPRS.'
              stop 99
          endif
      else if(mmode .eq. 5) then
          if(kc5 .le. 0) then
              write(lo,*)'file:',name,' missing CTDPRS.'
              stop 99
          endif
      else
          write(lo,*)'prgrmr err. fell out of ifblk chking kcX vals.' 
          stop 99
      endif
c
c -- read data headers and asterisk fields.
      nast = 0
      j = -7
      do 180 i=1,ncinfi
         j = j + 8
         colnam(i) = hline(2)(j:j+7)
         coluni(i) = hline(3)(j:j+7)
         colast(i) = hline(4)(j:j+7)
         if(colast(i) .eq. ' *******') then
c            -- this column has a quality flag
             nast = nast + 1
             qx(i) = nast
         else
             qx(i) = -1
         endif
 180  continue
c
c -- print list of headers.  ncinfi==number of columns in input file.
      write(lo,*)'found',ncinfi,' data columns.',nast,' had asterisks.'
      write(lo,2060) (colnam(j),j,j=1,ncinfi)
 2060  format(5(a8,'<',i2))
c
 200  write(lo,*)'do you want to Merge into an existing column or',
     *           ' Add a new one? (m/a):'
      read(li,2010) c1
      if(c1.eq.'m' .or. c1.eq.'M') then
          add = .false.
      else if(c1.eq.'a' .or. c1.eq.'A') then
          add = .true.
      else
          write(lo,*)'please enter M or A.'
          go to 200
      endif
c
      if(.not.add) then
c         -- merging.
 208      write(lo,*)'enter number from list above for value you',
     *               ' want to merge into:'
          read(li,*) a
          ctm = nint(a)
          if(ctm .lt. 1 .or. ctm.gt.ncinfi) then
              write(lo,*)'choice out of range. max= ',ncinfi,
     *                   'try again.'
              go to 208
          endif
          if(qx(ctm) .gt. 0) then
c             -- column to merge has a quality flag.
              if(naqf .eq. nef) then
c                 -- number of quality flags match. good.
              else if(naqf .eq. 0) then
                  write(lo,*)'will leave quality flag(s) of replaced',
     *                       ' values asis. (no Q flags in new file)'
              else if(naqf.eq.1 .and. nef.eq.2) then
                  write(lo,*)'new file has 1 qual flag while existing',
     *                       ' file has 2 qual words.'
                  write(lo,*)'will modify QUALT1 flag and leave',
     *                       ' QUALT2 flag asis.'
              else if(naqf.eq. 2 .and. nef.eq. 1) then
                  write(lo,*)'existing file has only 1 qual word.',
     *                       ' ignoring 2nd qual flag in new file.' 
              else
                  write(lo,*)'prgrmr err. in merge qual flag check.'
                  stop 99
              endif
          else
c             -- column to merge has no quality flag.
              if(naqf .ne. 0) then
                  write(lo,*)'existing .sea file has no quality',
     *                       'flag for:',colnam(ctm)
                  write(lo,*)'but you said new file has ',naqf,
     *                       ' quality flags. ??  aborting.'
                  stop 99
              endif
          endif
c         -- read first line of data and figure out format of value
c            that is being merged into.
          read(ioa,2010) line
          i = (ctm-1)*8 + 1
          c8 = line(i:i+7)
          do 220 i=8,3,-1
             if(c8(i:i) .eq. '.') then
                 write(ofmt,2085) 8-i
 2085             format('(f8.',i1,')')
                 go to 226
             endif
 220      continue
          write(lo,*)'can''t figure out output format required.'
 224      write(lo,*)'how many decimal digits to print ',colnam(ctm),
     *               '? (0-5):'
          read(li,*) a
          i =  nint(a)
          if(i.lt.0 .or. i.gt.5) then
              write(lo,*)'out of range. enter number 0-5:'
              go to 224
          endif
          write(ofmt,2085) i
 226      continue
          write(lo,*)' using format ',ofmt,' for merged data values.'
c         -- rewind, then skip forward 4 again
          rewind(ioa)
          do 230 i=1,4
             read(ioa,2010) line
 230      continue
      else
c         -- adding a new column.
 238      write(lo,*)'enter a 7 (or less) character name for the ',
     *               'value you wish to add:'
          read(li,2010) c8
c         -- upper case, then left justify the name and see if it's unique.
          call ucase(c8)
          call ljustn(c8,j)
          do 240 i=1,ncinfi
             k = index(colnam(i),c8(1:7))
             if(k .gt. 0) then
c                -- have found matching name.
                 write(lo,*)'name entered is not unique. try again.'
                 go to 238
             endif
 240      continue
          call rjust(c8)
          newnam = c8
c
c         -- deal with quality flag things.
          qfph = .false.
          if(naqf .eq. nef) then
c             -- number of new quality flags matches number of existing. good.
          else if(naqf .eq. 0) then
 250          write(lo,*)'new file contains no quality flags.'
              write(lo,*)'Keep it ''Asis'' or set quality',
     *                   'flags to Placeholders? (A/P):'
              read(li,2010) c1
              if(c1.eq.'a' .or. c1.eq.'A') then
c                 -- new value really has no quality flag.
              else if(c1.eq.'P' .or. c1.eq.'p') then
                  qfph = .true.
              else
                  write(lo,*)'please enter A or P.'
                  go to 250
              endif
          else if(naqf.eq.1 .and. nef.eq.2) then
              write(lo,*)'new file has 1 qual flag while existing',
     *                   ' file has 2 qual words.'
              write(lo,*)'I will modify QUALT1 flag and set ',
     *                   'QUALT2 flag to 1.' 
          else if(naqf.eq.2 .and. nef.eq.1) then
              write(lo,*)'existing file only has 1 qual word.',
     *                   ' ignoring 2nd qual flag in new file.'
          else
               write(lo,*)'prgrmr err. in add qual flag check.'
               stop 99
          endif
c         
c         -- more questions when you need to add a column.
          write(lo,*)'What units are ',newnam,' in? (7 char max):'
          read(li,2010) newuni
          call ucase(newuni)
          call rjust(newuni)
c
 280      write(lo,*)'how many decimal digits to print ',newnam, 
     *               '? (0-5):'
          read(li,*) a
          i =  nint(a)
          if(i.lt.0 .or. i.gt.5) then
              write(lo,*)'out of range. enter number 0-5:'
              go to 280
          endif
          write(ofmt,2085) i
c
 282      write(lo,*)'Is missing value -9.0 or -999.0 ? (-9/-999):'
          read(li,*) a
          if(icd .ne. 0) then
              write(lo,*)' enter -9 or -999.  try again.'
              go to 282
          else if(a.gt.-10. .and. a.lt.10.) then
              misval = -9.0
          else if(a.gt.-1000. .and. a.lt.1000.) then
              misval = -999.0
          else
              write(lo,*)'invalid number. enter -9 or -999. try again.'
              go to 282
          endif
      endif
c
c -- ask for output filename.
 290  write(lo,*)'enter output filename:'
      read(li,2010) oname
      open(iob,file=oname,status='new',iostat=icd)
      if(icd .ne. 0) then
          write(lo,*)'can''t open ',oname,' iostat=',icd
          inquire(file=oname,exist=xist,iostat=icd)
          if(icd .ne. 0) then
              write(lo,*)'inquire failed on output file.'
              stop 99
          endif
          if(xist) then
              write(lo,*)'must enter a file that doesn''t exist.'
              write(lo,*)'try again.'
              go to 290
          endif
      endif
c
c -- take care of headers.
      if(.not. add) then
c         -- not adding anything. just replacing. copy headers to
c            output file without modification.
          write(iob,2010) hline(1)(1:eol)
          write(iob,2010) hline(2)(1:eol)
          write(iob,2010) hline(3)(1:eol)
          write(iob,2010) hline(4)(1:eol)
c
c         -- figure out where replaced things will be.
          rb1 = (ctm-1)*8 + 1
          re1 = rb1 + 7
          iqf1 = -1
          iqf2 = -1
          if(qx(ctm) .ge. 1) then
c             -- this parameter does have a quality flag.
              iqf1 = (q1+5) - (nast-qx(ctm))
              if(q2 .ge. 1) then
                  iqf2 = (q2+5) - (nast-qx(ctm))
              endif
          endif
      else
c         -- adding a column. 
          nnast = nast
          if(naqf.gt.0 .or. qfph) nnast = nast + 1
          widqf = max(7,nnast+1)
          neol = eol + 8 + widqf
          if(nef .eq. 2) neol = neol + widqf
c         -- print the first header.
          write(iob,2010) hline(1)(1:neol)
c         -- figure out the 2nd header.  (names)
          if(nef .eq. 1) then
              c40 = ' '
              c40(1:widqf) = hline(2)(ldc+1:q1+5)
              call rjust(c40(1:widqf))
              write(iob,2100) hline(2)(1:ldc),newnam,c40(1:widqf)
 2100          format(a,a8,a,a)
          else if(nef .eq. 2) then
              c40 = ' '
              c40(1:widqf) = hline(2)(ldc+1:q1+5)
              call rjust(c40(1:widqf))
              d40 = ' '
              d40(1:widqf) = hline(2)(q1+6:q2+5)
              call rjust(d40(1:widqf))
              write(iob,2100) hline(2)(1:ldc),newnam,c40(1:widqf),
     *                        d40(1:widqf)
          else
              write(lo,*)'prgrmr err. bad nef in header print=',nef
          endif
c         -- figure out the 3rd header. (units)
          write(iob,2100) hline(3)(1:ldc),newuni,hline(3)(ldc+9:neol)
c         -- figure out the 4th header (asterisks)
          c8 = ' '
          if(naqf.gt.0 .or. qfph) c8 = ' *******'
          write(iob,2100) hline(4)(1:ldc),c8,hline(4)(ldc+9:neol)
      endif
c     -- done with headers.
c
c -- top of data reading loop.
      ilnum = 4
 300  continue
      read(ioa,2010,iostat=icd) line
      if(icd .ne. 0) then
          if(icd .lt. 0) then
c             -- normal eof.  close files and go to clean up section.
              close(ioa)
              close(iob)
              go to 500
          else
c             -- some sort of data reading error.
              write(lo,*)'read err on existing .sea file. iostat=',icd
              write(lo,*)' after line number',ilnum
              stop 99
          endif
      endif
      ilnum = ilnum + 1
c -- decode required portions.
      estn = line(kc1:kc1+7)
      call ljustn(estn,i)
      if(mmode .eq. 1) then
          read(line(kc2:kc2+7),'(i8)',iostat=icd) ecastn
          if(icd .ne. 0) then
              write(lo,*)'err decoding CASTNO on this line=',line(1:eol)
              stop 99
          endif
          esamp = line(kc3:kc3+7)
          call ljustn(esamp,i)
      else if(mmode .eq. 2) then
          read(line(kc2:kc2+7),'(i8)',iostat=icd) ecastn
          if(icd .ne. 0) then
              write(lo,*)'err decoding CASTNO on this line=',line(1:eol)
              stop 99
          endif
          ebotnm = line(kc4:kc4+7)
          call ljustn(ebotnm,i)
      else if(mmode .eq. 3) then
          esamp = line(kc3:kc3+7)
      else if(mmode .eq. 4) then
          read(line(kc2:kc2+7),'(i8)',iostat=icd) ecastn
          if(icd .ne. 0) then
              write(lo,*)'err decoding CASTNO on this line=',line(1:eol)
              stop 99
          endif
          read(line(kc5:kc5+7),'(f8.1)',iostat=icd) epress
          if(icd .ne. 0) then
              write(lo,*)'err decoding CTDPRS on this line=',line(1:eol)
              stop 99
          endif
      else if(mmode .eq. 5) then
          read(line(kc5:kc5+7),'(f8.1)',iostat=icd) epress
          if(icd .ne. 0) then
              write(lo,*)'err decoding CTDPRS on this line=',line(1:eol)
              stop 99
          endif
      else
          write(lo,*)'prgrmr err. fell out of ifblk mmode aft st300.'
          stop 99
      endif
c
c -- see if any match is in the new input file. 
      call match(astn,istn,acastn,asamp,isamp,abotnm,ibotnm,apress,
     *           estn,ecastn,esamp,ebotnm,epress,mmode,nndl,
     *           prslim,idp)
      if(idp .le. 0) then
c         -- no match found. continue on.
      else
c         -- match found.  see if it's been used before and then mark it used.
          if(used(idp)) then
              write(lo,*)'used twice! stn,cast,sampno=',astn(idp),
     *                   ' ',acastn(idp),' ',asamp(idp)
          else
              used(idp) = .true.
          endif
      endif
c
      if(.not.add) then
c         -- replacing data
          if(idp .gt. 0) then
c             -- match is available.
              write(c8,ofmt,iostat=icd) val(idp)
              if(icd .ne. 0) then
                  write(lo,*)'write err. ofmt=',ofmt,' val=',val(idp)
                  write(lo,*)'was trying to modify this line=',line
                  stop 99
              endif
              line(rb1:re1) = c8
              if(iqf1.gt.0 .and. naqf.ge.1)
     *            write(line(iqf1:iqf1),'(i1)') qf(idp,1)
              if(iqf2.gt.0 .and. naqf.ge.2)
     *            write(line(iqf2:iqf2),'(i1)') qf(idp,2)
          else
c             -- no match available. just print out the line unchanged.
          endif
          write(iob,2010,iostat=icd) line(1:eol)
          if(icd .ne. 0) then
              write(lo,*)'write error on ',oname,' iostat=',icd
              stop 99
          endif
      else
c         -- adding a column of data.
          if(idp .gt. 0) then
c             -- match found
              write(c8,ofmt,iostat=icd) val(idp)
              if(icd .ne. 0) then
                  write(lo,*)'write err. ofmt=',ofmt,' val=',val(idp)
                  write(lo,*)'was trying to add to this line=',line
                  stop 99
              endif
              if(val(idp) .lt. misval+.01) then
                  tqf1 = 9
                  tqf2 = 9
                  if(qf(idp,1).eq.5) then
c                     -- special case missing.
                      tqf1 = 5
                      tqf2 = 5
                  endif
              else if(qfph) then
                  tqf1 = 1
                  tqf2 = 1
              else 
                  tqf1 = qf(idp,1)
                  if(naqf.eq.1 .and. nef.eq.2) then
                      tqf2 = 1
                  else
                      tqf2 = qf(idp,2)
                  endif
              endif
          else
c             -- match not found. must add fake data and flags in the column.
              write(c8,ofmt,iostat=icd) misval
              if(icd .ne. 0) then
                  write(lo,*)'write err. ofmt=',ofmt,' val=',misval
                  write(lo,*)'was trying to add to this line=',line
                  stop 99
              endif
              tqf1 = 9
              tqf2 = 9
          endif
c
          if(naqf.le.0 .and. .not.qfph) then
c             -- added data column has no real or placeholder quality flags.
              write(iob,2200,iostat=icd) line(1:ldc),c8,
     *                                   line(ldc+1:eol)
 2200          format(a,a8,a,a)
              if(icd .ne. 0) then
                  write(lo,*)'write error on ',oname,' iostat=',icd
                  stop 99
              endif
          else if(naqf.ge.1 .or. qfph) then
c             -- added data column has quality flags (real or not).
              write(c1,'(i1)') tqf1
              c40 = line(ldc+1:q1+5)
cc              write(*,*)'debug:tqf1,c40,widqf=',tqf1,c40,widqf
              call ljustn(c40,i)
cc              write(*,*)'debug:c40,i,c1=',c40,i,c1
              c40(i+1:i+1) = c1
cc              write(*,*)'debug:tqf1,c40,i,c1=',tqf1,c40,i,c1
              call rjust(c40(1:widqf))
cc              write(*,*)'debug:tqf1,c40,widqf=',tqf1,c40,widqf
              if(nef .eq. 1) then
c                 -- only one quality word.
                  write(iob,2200,iostat=icd) line(1:ldc),c8,
     *                                       c40(1:widqf)
                  if(icd .ne. 0) then
                      write(lo,*)'write error on ',oname,
     *                           ' iostat=',icd
                      stop 99
                  endif
              else 
                  write(c1,'(i1)') tqf2
                  d40 = line(q1+6:eol)
                  call ljustn(d40,i)
                  d40(i+1:i+1) = c1
                  call rjust(d40(1:widqf))
                  write(iob,2200,iostat=icd) line(1:ldc),c8,
     *                            c40(1:widqf),d40(1:widqf)
                  if(icd .ne. 0) then
                      write(lo,*)'write error on ',oname,
     *                           ' iostat=',icd
                      stop 99
                  endif
              endif
          else
              write(lo,*)'prgrmr err. fell through data adding block.'
              stop 99
          endif
      endif
c
c -- go back up and read more.
      go to 300
c
c -- get to here when all done.
 500  continue
      j = 0
      do 510 i=1,nndl
         if(.not.used(i)) j = j + 1
 510  continue
      if(j.gt.0) then
 518      write(lo,*)j,' samples from new file were not used.'
          write(lo,*)' do you want to list them? (y/n):'
          read(li,2010) c1
          if(c1.eq.'y' .or. c1.eq.'Y') then
              write(lo,*)'enter filename to print unused info.:'
              read(li,2010) name
              open(iob,file=name,status='new',iostat=icd)
              if(icd .ne. 0) then
                  write(lo,*)'err opening ', name,' try a new name.'
                  go to 518
              endif
              if(mmode .eq. 1) then
                do 540 i=1,nndl
                   if(.not.used(i))
     *                 write(iob,*) astn(i),'/',acastn(i),' ',asamp(i)
 540            continue
              else if(mmode .eq. 2) then
                do 542 i=1,nndl
                   if(.not.used(i))
     *                 write(iob,*) astn(i),'/',acastn(i),' ',abotnm(i)
 542            continue
              else if(mmode .eq. 3) then
                do 544 i=1,nndl
                   if(.not.used(i))
     *                 write(iob,*) astn(i),' ',asamp(i)
 544            continue
              else if(mmode .eq. 4) then
                do 546 i=1,nndl
                   if(.not.used(i))
     *                 write(iob,*) astn(i),'/',acastn(i),apress(i)
 546            continue
              else if(mmode .eq. 5) then
                do 548 i=1,nndl
                   if(.not.used(i))
     *                 write(iob,*) astn(i),' ',apress(i)
 548            continue
              else
                   write(lo,*)'prgrmr err. fell out of mmode blk',
     *                        ' after st510.'
              endif
              close(iob)
          else
c             -- don't care about samples not used.
          endif
      endif
      stop
      end
c **************************************************************
      subroutine nschar(string,n)
c -- find last non-blank character in a string. returns the position.
c    returns 0 if all blank.
      character string*(*)
      integer n,nc,i
c
      intrinsic len
      nc = len(string)
      do 10 i=nc,1,-1
         if(string(i:i) .ne. ' ') then
             n = i
             return
         endif
 10   continue
      n = 0
      return
      end
