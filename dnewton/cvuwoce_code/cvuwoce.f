c program cvuwoce converts ML/L and UMOL/L units in WOCE format data to /KG.
c
      integer li,lo,ioa,iob,icd
      character name*40
      integer maxdat
      parameter (maxdat=30)
      character*300 head1,head2,head3,line
      character ch1*1,c8*8
      integer eol,eop,ntc,i,j,k,ctc(maxdat),isal,itmp,nlw,ndp(maxdat)
      real s,t,pden,sigt,v
      logical yesoxy,whole,nosal,notemp,tmiss
      intrinsic index,mod
      external sigmp
c
      data li/5/,lo/6/,ioa/12/,iob/14/
c
      write(lo,*)'cvuwoce converts WOCE format L/L units to /KG.'
      write(lo,*)'enter input (.sea) filename:'
      read(li,2010) name
 2010  format(a)
      open(ioa,file=name,status='old')
c
c -- read and save top line
      read(ioa,2010) head1
      if(index(head1,'EXPOCODE') .eq. 0) then
          write(lo,*)' 1st line contains no EXPOCODE string.'
          write(lo,*)' This is not WOCE format data. aborting.'
          stop 99
      endif
c
c -- read second line (parameter names) and figure out line length.
      read(ioa,2010) head2
      eol = index(head2,'QUALT1') 
      if(eol .eq. 0) then
          write(lo,*)' 2nd line contains no QUALT1 string.'
          write(lo,*)' This is not WOCE format data or the lines',
     *               ' are too long. aborting.'
          stop 99
      endif
      eol = eol + 5
c     -- search backward on line or non-blank.
      do 20 i=eol-6,1,-1
         if(head2(i:i) .ne. ' ') then
c             -- found last column used by data. EndOfParameters
              eop = i
              if(mod(eop,8) .ne. 0) then
                  write(lo,*)' Looks like data names are not on 8',
     *                       ' character boundaries. aborting.'
                  stop 99
              endif
              go to 22      
         endif
 20   continue
      write(lo,*)'something''s wrong in 20 lp. fell through bottom.'
      stop 99
 22   continue
c
      i = index(head2,'QUALT2')
      if(i .ne. 0) then
c         -- found a QUALT2.  this is the real eol.
          eol = i + 5
      endif
c
c -- I'm convinced this is WOCE data. now ask for output filename.
c
      write(lo,*)'enter output filename:'
      read(li,2010) name
      open(iob,file=name,status='new')
c
c -- write those first 2 lines to output
      write(iob,2010) head1(1:eol)
      write(iob,2010) head2(1:eol)
c
c -- read the unit line.
      read(ioa,2010) head3
      ntc = 0
      yesoxy = .false.     
      do 40 k=1,maxdat
         i = (k-1)*8 + 1
         j = i + 7
         if(i .ge. eop) then
c            -- have reached end of data on line.
             go to 44
         endif
         if(index(head3(i:j),'L/L') .ne. 0) then
c            -- have found unit with L/L in it.  Add to Number To Convert.
             ntc = ntc + 1
             ctc(ntc) = i
             if(head3(i+6:i+6) .ne. '/') then
                 write(lo,*)head2(i:j),' unit label not properly',
     *                      ' placed. aborting.'
                 stop 99
             endif
             write(lo,2040) head2(i:j),head3(i:j)
 2040         format(1x,a8,1x,a8)
             if(index(head2(i:j),'OXY') .ne. 0) then
                 if(index(head2(i:j),'CTDOXY').ne.0) then
c                    -- this row of oxygen is CTD
                 else
c                    -- this row of oxygens is bottle. set flag
                     yesoxy = .true.
                 endif
                 ndp(ntc) = 1
             else if(index(head2(i:j),'SIL') .ne. 0) then
                 ndp(ntc) = 2
             else if(index(head2(i:j),'NITRA') .ne. 0) then
                 ndp(ntc) = 2
             else if(index(head2(i:j),'NITRI') .ne. 0) then
                 ndp(ntc) = 2
             else if(index(head2(i:j),'NO2+N') .ne. 0) then
                 ndp(ntc) = 2
             else if(index(head2(i:j),'PHSPH') .ne. 0) then
                 ndp(ntc) = 2
             else
                 write(lo,*) head2(i:j),' unexpected conversion.'
                 write(lo,*)' How many points past decimal',
     *                      ' in output?:'
                 read(li,*) ndp(ntc)
             endif
         endif
 40   continue
      write(lo,*)'data line too long. fell through 10lp. maxdat=',
     *            maxdat,'  aborting.'
      stop 99
c
 44   continue
      if(ntc .eq. 0) then
          write(lo,*)' No units to change found!'
          write(lo,*)' Output file is incomplete.'
          stop
      else
          write(lo,*)'Found ',ntc,' data columns to convert'
          if(yesoxy) then
              write(lo,*)'  Bottle oxygen was one of them.'
          else
              write(lo,*)'  Bottle oxygen was NOT one of them.'
          endif
          write(lo,*)' <enter> to continue; Q to quit:'
          read(li,2010) ch1
          if(ch1 .eq. 'q' .or. ch1 .eq. 'Q') then
              write(lo,*)' Program quit.'
              stop
          endif
      endif
c
c -- fix the units line and print it out.
      do 50 k=1,ntc
         i = ctc(k)
         j = i + 7
         if(index(head2(i:j),'OXY').ne.0) then
             head3(i:j) = ' UMOL/KG'
         else
c            -- need a U,P,or N in MOLS string.
             c8 = ' ' // head3(i+2:i+2) // 'MOL/KG'
             head3(i:j) = c8
         endif
 50   continue
      write(iob,2010) head3(1:eol)
c       
c -- ask about oxygen method.
      if(yesoxy) then
 70       continue
          write(lo,*)' Were bottle oxygens Whole bottle ',
     *               ' or Aliquot? (W/A):'
          read(li,2010) ch1
          if(ch1.eq.'w' .or. ch1.eq.'W') then
              whole = .true.
          else if(ch1.eq.'a' .or. ch1.eq.'A') then
              whole = .false.
              write(lo,*)'Will use temp=25. for oxygen conversion.'
          else
              write(lo,*)' enter W or A.'
              write(lo,*)' In truth it probably doesn''t matter.'
              go to 70
          endif
      endif
c
c -- locate salinity and temperature columns.
c
      nosal = .false.
      if(index(head2,'CTDSAL') .ne. 0) then
          isal = index(head2,'CTDSAL') - 2
      else if(index(head2,'SALNTY') .ne. 0) then
          isal = index(head2,'SALNTY') - 2
      else
          write(lo,*)' no salinity found. using 34.8 .'
          nosal = .true.
      endif
c -- test position of salinity
      if(.not. nosal) then
          if(mod(isal-1,8) .ne. 0) then
              write(lo,*)' salinity or label not on 8 byte boundary.'
              write(lo,*)' fix it.  aborting.'
          endif
      endif
c
      notemp = .false.
      if(index(head2,'CTDTMP') .ne. 0) then
          itmp = index(head2,'CTDTMP') - 2
      else if(index(head2,'THETA') .ne. 0) then
          itmp = index(head2,'THETA') - 3
      else if(index(head2,'REVTMP') .ne. 0) then
          itmp = index(head2,'REVTMP') - 2
      else
          write(lo,*)' no temperature found. using approximations.'
          notemp = .true.
      endif
c -- test position of temperature
      if(.not. notemp) then
          if(mod(itmp-1,8) .ne. 0) then
              write(lo,*)' temper. or label not on 8 byte boundary.'
              write(lo,*)' fix it.  aborting.'
          endif
      endif
c
c read and copy silly asterisk line.
      read(ioa,2010) line
      write(iob,2010) line(1:eol)
c
c -- read all data lines converting one at a time.
      nlw = 4
 80   continue
      read(ioa,2010,end=100) line
c     -- extract salinity.
      if(nosal) then
          s = 34.8
      else
          c8 = line(isal:isal+7)
          read(c8,'(f8.3)',iostat=icd) s
          if(icd .ne. 0) then
              write(lo,*)' read err in salinity. aborting. rec#=',
     *                     nlw+1,' line=' 
              write(lo,*) line
              stop 99
          endif
          if(s .le. 0.0) then
              s = 34.8
          else if(s .lt. 20. .or. s.gt. 60.) then
              write(lo,*)' salinity is ridiculous=',s,' rec#=',nlw+1
          endif
      endif
c     -- extract temperature.
      tmiss = .true.
      if(notemp) then
      else
          c8 = line(itmp:itmp+7)
          read(c8,'(f8.4)',iostat=icd) t
          if(icd .ne. 0) then
          write(lo,*)' read err in temp. aborting. rec#=',
     *                     nlw+1,' line='
              write(lo,*) line
              stop 99
          endif
          if(t .gt. -3.0) tmiss = .false.
      endif
c
      do 90 k=1,ntc
         i = ctc(k)
         j = i + 7
         c8 = line(i:j)
         read(c8,'(f8.1)',iostat=icd) v
         if(icd .ne. 0) then
             write(lo,*)'unreadable value. aborting. rec#=',nlw+1,
     *                  ' line=',line
             stop 99
         endif
         if(v .lt. -3.0) then
c            -- missing
             v = -9.0
         else if(index(head2(i:j),'OXY') .ne. 0) then
c            -- dealing with oxygen.
             if(.not.whole .and.
     *           index(head2(i:j),'CTDOXY').eq.0) then
c                 -- not CTDOXY and yes oxybootles were aliquot.
                  t = 25.0
             else if(tmiss) then
                  t = 25.0
                  write(lo,*)'T missing. using 25. at rec#=',nlw+1
             endif
             call sigmp(0.0,0.0,t,s,sigt)
             v = v / (0.022392 * (sigt/1000.0 + 1.0))
         else
c            -- everything, but oxygen
             call sigmp(0.0,0.0,25.0,s,pden) 
             v = v / (pden/1000. + 1.0)
         endif
c        -- done converting. print to string.
         if(ndp(k) .eq. 2) then
             write(c8,'(f8.2)') v
         else if(ndp(k) .eq. 1) then
             write(c8,'(f8.1)') v
         else if(ndp(k) .eq. 3) then
             write(c8,'(f8.3)') v
         else if(ndp(k) .eq. 4) then
             write(c8,'(f8.4)') v
         else
             write(lo,*)' can''t print number with',ndp(k),' dec point.'
             stop 99
         endif
         line(i:j) = c8
 90   continue
      write(iob,2010) line(1:eol)
      nlw = nlw + 1
      go to 80
c
 100  continue
c     -- get here on normal end of file.
      write(lo,*)nlw,' lines written.'
      close(ioa)
      close(iob)
      stop
      end
