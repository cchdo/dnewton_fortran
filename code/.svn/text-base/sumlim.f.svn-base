      subroutine sumlim(le,ioa,bg,en)
c
c -- routine to read the 2nd and 3rd header from a WOCE .SUM file and
c    return two arrays giving the beginning and end columns for the
c    usual things in a .SUM file.
c author: David Newton. Scripps Instn. of Oceanog.
c
      integer mxhdng
      parameter (mxhdng=19)
      integer le,ioa,bg(mxhdng),en(mxhdng)
c
c le     ->  fortran error unit.
c ioa    ->  fortran unit for .SUM file. already opened. expects to read 2nd
c             header right away.
c bg     <- integer array filled with start positions of .SUM info.
c             ie  bg(1) = start position of EXPOCODE.  Indecies below.
c en     <- same as bg for ending column of info.
c
c  parameters that are NOT found will have bg(n)==0
c
c
c   1==EXPOCODE    6==DATE       12==BOTTOM_DEPTH (uncorrected)
c   2==SECTION     7==TIME       13==HT_ABOVE_BOTTOM
c   3==STNNBR      8==TIME_CODE  14==METER_WHEEL
c   4==CASTNO      9==LATITUDE   15==MAX_PRESSURE
c   5==CAST_TYPE  10==LONGITUDE  16==NO_OF_BOTTLES
c                 11==NAV_TYPE   17==PARAMS_COLLECTED
c                                18==COMMENTS
c                                19==BOTTOM_DEPTH (corrected)
c
c changes:
c 18aug1993 dmn. bug in finding uncorrected depth fixed by uncommenting a line.
c 18nov1993 dmn. modified to recognize CDEPTH and warn on inproper corrected
c                  depth heading.  will still recognize COR on 1st line.
c 09feb1994 dmn. changed default depth to corrected.
c 24Aug1995 dmn. changed method for corrected depth finding. bug fix.
c
      logical inword,trutab(2)
      integer i,j,k,icd,idum(mxhdng),tbg(2)
      character line*200,line2*200,hdng(mxhdng)*8
c
      intrinsic index
c
c -- first 3 to 5 characters are used to find headings.
      data hdng/'EXPOCODE','SECTION ','STNNBR  ','CASTNO  ',
     *          'TYPE    ','DATE    ','TIME    ',' CODE   ',
     *          'LATITUDE','LONGITUD','NAV_CODE','DEPTH_UN',
     *          'BOTTOMHT','WHEEL_MT','PRESS_MX','BOTTLES ',
     *          'PARAMS  ','COMMENTS','CDEPTH  '/
c
c
      read(ioa,2010,iostat=icd) line2
 2010  format(a)
      if(icd .ne. 0) then
          write(le,*)'sumlim: err trying to read 2nd header.',
     *               'iostat=',icd
          stop 99
      endif
c
      if(index(line2,'POS') .ne. 0) then
c         -- okay
      else if(index(line2,'osit') .ne. 0) then
c         -- mixed case, but okay.
          write(le,*)'sumlim: 2nd header has lower case.'
      else
          write(le,*)'sumlim: couldn''t recognize 2nd header, but',
     *               ' will try to continue.'
      endif
c
c
c -- read 3rd header.
c
      read(ioa,2010,iostat=icd) line
      if(icd .ne. 0) then
          write(le,*)'sumlim: err trying to read 3rd header.',
     *               'iostat=',icd
          stop 99
      endif
c
      if(index(line,'EXPO') .eq. 0) then
          write(le,*)'sumlim: expecting 3rd header. looking for',
     *               ' ''EXPO''. got=',line
          stop 99
      endif
c
c -- looks like we're in the right place. decode it.
      bg(1) = index(line,hdng(1)(1:4))
      bg(2) = index(line,hdng(2)(1:4))
CC      if(bg(2).eq.0) bg(2)=index(line,'WHP')
      bg(3) = index(line,hdng(3)(1:4))
      bg(4) = index(line,hdng(4)(1:4))
      bg(5) = index(line,hdng(5)(1:4))
      bg(6) = index(line,hdng(6)(1:4))
      bg(7) = index(line,hdng(7)(1:4))
      bg(8) = index(line,hdng(8)(1:5))
      if(bg(8) .ne. 0) bg(8) = bg(8) + 1
      bg(9) = index(line,hdng(9)(1:3))
      bg(10) = index(line,hdng(10)(1:3))
      bg(11) = index(line,hdng(11)(1:3))
CC      bg(12) = index(line,hdng(12)(1:4))
      bg(13) = index(line,hdng(13)(1:5))
      bg(14) = index(line,hdng(14)(1:4))
      bg(15) = index(line,hdng(15)(1:4))
      bg(16) = index(line,hdng(16)(1:5))
      bg(17) = index(line,hdng(17)(1:4))
      bg(18) = index(line,hdng(18)(1:3))
c
      tbg(1) = index(line,hdng(12)(1:4))
      if(tbg(1) .eq. 0) then
c         -- no bottom depths at all.
          tbg(2) = 0
      else
c         -- found one bottom depth column. look for another one.
          tbg(2) = index(line(tbg(1)+2:200),hdng(19)(1:4))
          if(tbg(2).ne.0) tbg(2) = tbg(2) + tbg(1) + 2
      endif
c
c -- now figure out which depth column is corrected and which is uncorrected.
      bg(12) = 0
      bg(19) = 0
      if(tbg(1).eq.0 .and. tbg(2).eq.0) then
c         -- no depth at all. drop through
      else if(line(tbg(1)-1:tbg(1)+2) .eq. 'CDEP' .or.
     *        line(tbg(2)-1:tbg(2)+2) .eq. 'CDEP') then
c         -- corrected depth for sure.
          trutab(1) = .false.
          trutab(2) = .false.
          if(line(tbg(1)-1:tbg(1)+2) .eq. 'CDEP') trutab(1)=.true.
          if(line(tbg(2)-1:tbg(2)+2) .eq. 'CDEP') trutab(2)=.true.
          if(trutab(1) .and. trutab(2)) then
              write(le,*)'found 2 corrected depth cols.',
     *                   ' will use first.'
              bg(19) = tbg(1) - 1
              bg(12) = 0
          else if(trutab(1)) then
              bg(19) = tbg(1) - 1
              if(tbg(2) .ne. 0) bg(12) = tbg(2)
          else
              bg(19) = tbg(2) - 1
              if(tbg(1) .ne. 0) bg(12) = tbg(1)
          endif
      else if(index(line2(tbg(1):tbg(1)+6),'COR') .ne. 0  .or.
     *        index(line2(tbg(2):tbg(2)+6),'COR') .ne. 0) then
c         -- found clue in 2nd header rather than 3rd.
          trutab(1) = .false.
          trutab(2) = .false.
          if(index(line2(tbg(1):tbg(1)+6),'COR')
     *             .ne. 0) trutab(1) = .true.
          if(index(line2(tbg(2):tbg(2)+6),'COR')
     *             .ne. 0) trutab(2) = .true.
          if(trutab(1) .and. trutab(2)) then
              write(le,*)'found 2 corrected depth cols.',
     *                   ' will use first.'
              bg(19) = tbg(1)
              bg(12) = 0
          else if(trutab(1)) then
              bg(19) = tbg(1)
              if(tbg(2) .ne. 0) bg(12) = tbg(2)
          else
              bg(19) = tbg(2)
              if(tbg(1) .ne. 0) bg(12) = tbg(1)
          endif
      else if(index(line2(tbg(1):tbg(1)+6),'UNC') .ne. 0  .or.
     *        index(line2(tbg(2):tbg(2)+6),'UNC') .ne. 0) then
c         -- found UNC in 2nd header.
          trutab(1) = .false.
          trutab(2) = .false.
          if(index(line2(tbg(1):tbg(1)+6),'UNC')
     *             .ne. 0) trutab(1) = .true.
          if(index(line2(tbg(2):tbg(2)+6),'UNC')
     *             .ne. 0) trutab(2) = .true.
          if(trutab(1) .and. trutab(2)) then
              write(le,*)' found 2 uncorrected depth cols.',
     *                   ' will use first.'
              bg(12) = tbg(1)
              bg(19) = 0
          else if(trutab(1)) then
              bg(12) = tbg(1)
              if(tbg(2) .ne. 0) then
                  write(le,*)' assuming depth at col:',tbg(2),
     *                       ' is corrected depth.'
                  bg(19) = tbg(2)
              endif
          else
              bg(12) = tbg(2)
              if(tbg(1) .ne. 0) then
                  write(le,*)' assuming depth at col:',tbg(1),
     *                       ' is corrected depth.'
                  bg(19) = tbg(1)
              endif
          endif
      else
c         -- no clues found. make assumptions.
          if(tbg(1).ne.0 .and. tbg(2).ne.0) then
              write(le,*)' two depth cols found.',
     *                   ' assume 2nd is corrected.'
              bg(12) = tbg(1)
              bg(19) = tbg(2)
          else if(tbg(1) .ne. 0) then
              write(le,*)' assuming depth is corrected.'
              bg(12) = 0
              bg(19) = tbg(1)
          else if(tbg(2) .ne. 0) then
              write(le,*)' assuming depth is corrected.'
              bg(12) = 0
              bg(19) = tbg(2)
          else
              write(le,*)'can''t get here. sumlim. CDEP ifblock.'
              write(le,*)' notify dnewton@ucsd.edu'
              stop 99
          endif
      endif
c
c -- make list of all parameters that were not found.
c
      j = 0
      do 10 i=1,mxhdng
         if(bg(i) .eq. 0) then
             j = j + 1
             idum(j) = i
         endif
 10   continue
c -- print the list.
      if(j.gt.0) write(le,2040) (hdng(idum(i)),i=1,j)
 2040 format(' .SUM header was missing: ',/,(6A9))
c
c -- go through line finding where any word starts.
      k = 0
      inword=.true.
c     -- changed end limit frm 132 to 200. dmn 9june93.
      do 20 i=bg(1)+1,200
         if(line(i:i).eq.' ') then
             inword = .false.
         else 
c            -- line(i:i) not blank.
             if(.not. inword) then
                 k = k + 1
                 if(k.gt.mxhdng) then
                     write(le,*)'sumlim: prgrmng err. in 20loop.',
     *                          'k=',k
                     stop 99
                 endif
                 idum(k) = i
             endif
             inword = .true.
         endif
 20   continue
c
c -- now take each bg element and find the beginning of some other element
c    that is just bigger than it. (that'll locate the end position)
c
c -- but first see that the COMMENT parameter is really #18
      if(hdng(18)(1:3).ne.'COM') then
c         -- algorithm will fail.
          write(le,*)'sumlim: prgrmng err. ''COM'' not 18.'
          stop 99
      endif
c
      do 40 i=1,mxhdng
         en(i) = 200
 40   continue
c
      do 50 i=1,mxhdng
         if(bg(i) .eq. 0) go to 50
         if(bg(18).ne.0 .and. bg(i) .gt. bg(18)) then
             write(le,*)'sumlim: COMMENT param is not last.' 
             stop 99
         endif
         if(i .eq. 18) then
             en(i) = 200
             go to 50
         endif
         do 49 j=1,mxhdng
            if(idum(j) .gt. bg(i)) then
                en(i) = idum(j) - 1
                go to 50
            endif
 49      continue
 50   continue
c
      return
      end
