c program seaorder. puts WOCE format .SEA files into ascending pressure
c  sequenced order.
c author: David Newton. Scripps Instn. of Oceanog.
c
      integer li,lo,le,ioa,iob,icd
      integer i,nswrt,nssrt,eol,irec,nol
      logical eof,invert
      character name*40
c
      integer maxlev
      parameter (maxlev=64)
      character line(maxlev)*300,linsav*300
      real p(maxlev),a
      integer ip(maxlev),indx(maxlev)
c
      integer mxnumo
      parameter (mxnumo=50)
      integer sbg(-1:mxnumo),sen(-1:mxnumo),qualps(-1:mxnumo),nast,cast,
     *        castsv
      character units(-1:mxnumo)*8,seaexp*14,stnnsv*6,stnn*6
c
      intrinsic nint
      external lmsort
c
      data li/5/,lo/6/,le/7/,ioa/10/,iob/12/
      data nssrt/0/,nswrt/0/
c
      write(lo,*)' program seaorder. puts casts from WOCE format .SEA'
      write(lo,*)'  files into ascending pressure sequenced order.'
c
      write(lo,*)' enter input .SEA (.hyd) filename:'
      read(li,2010) name
 2010  format(a)
      open(ioa,file=name,status='old')
c
      write(lo,*)' enter output .SEA (.hyd) filename:'
      read(li,2010) name
      open(iob,file=name,status='new')
c
c -- read first 4 lines.
c
      do 10 i=1,4
         read(ioa,2010) line(i)
 10   continue
c
c -- rewind input and then call sealim.
c
      rewind(ioa)
c
      call sealim(ioa,le,sbg,sen,qualps,nast,units,seaexp,icd)
      if(icd .ne. 0) then
          write(le,*)'main: err rtn frm sealim. icd=',icd
          stop 99
      endif
c
c -- get end_of_line from qualt2 or qualt1
      if(sbg(0) .ne. 0) then
c         -- qualt2 was present. use it as end of line.
          eol = sen(0)
      else if(sbg(-1) .ne. 0) then
c         -- use qualt1.
          eol = sen(-1)
      else
          write(le,*)'main: no qualt1 or qualt2 to find eol with.'
          stop 99
      endif
c
c -- copy those first 4 lines to the output.
      do 12 i=1,4
         write(iob,2010) line(i)(1:eol)
 12   continue
      irec = 4
c
c -- read the very first data line.
c
 14   continue
      irec = irec + 1
      read(ioa,2010) line(1)
 16   call seaqwk(line(1),le,sbg,sen,stnnsv,castsv,icd)
      if(icd .ne. 0) then
          write(le,*)'main: skipping .SEA line. irec=',irec
          go to 14
      endif
      nol = 1
c
      eof = .false.
c     -- top of reading loop.
 20   continue
      irec = irec + 1
      read(ioa,2010,iostat=icd) line(nol+1)
      if(icd .ne. 0) then
          if(icd .lt. 0) then
c             -- hit end of file.  go to sorting.
              eof = .true.
              go to 50
          endif
          write(le,*)'main: read err. iostat=',icd
          stop 99
      endif
      call seaqwk(line(nol+1),le,sbg,sen,stnn,cast,icd)
      if(icd .ne. 0) then
          write(le,*)'main: skipping .SEA line. irec=',irec
          go to 20
      endif
      nol = nol + 1
c
      if(stnn .eq. stnnsv .and. cast .eq. castsv) then
c         -- still in same  station and cast.
          go to 20
      else
c         -- different cast or station.
          linsav = line(nol)
          nol = nol - 1
          go to 50
      endif
      write(le,*)'main: can''t get here aft stn/cast compare.'
      stop 99
c
c -- get here with nol lines stored.
c
 50   continue
c
c -- read the lines and get the pressure.
c
      a = -100.
      invert = .false.
      do 60 i=1,nol
         read(line(i)(sbg(6):sen(6)),'(bn,f8.1)',iostat=icd) p(i)
         if(icd .ne. 0) then
             write(le,*)'main: press decode err. line=',line(i)
             stop 99
         endif
         if(p(i) .lt. a) then
             invert = .true.
         else
             a = p(i)
         endif
 60   continue
c
      if(.not. invert) then
c         -- nothing has to be fixed.
          do 70 i=1,nol
             write(iob,2010) line(i)(1:eol)
 70       continue
          nswrt = nswrt + 1
          if(eof) go to 200
          line(1) = linsav
c         -- go up and decode the first line and read some more.
          go to 16
      endif
c
c -- get here with stations that need to be sorted.
c
c     -- initialize index and for integer pressure.
      do 80 i=1,nol
         indx(i) = i
         ip(i) = nint(p(i)*10.)
 80   continue
c
c -- call the sort
c
      call lmsort(ip,nol,indx)
      nssrt = nssrt + 1
c
c -- print the line using the index.
c
      do 90 i=1,nol
         write(iob,2010) line(indx(i))(1:eol)
 90   continue
      nswrt = nswrt + 1
c
c
 200  continue
      if(eof) then
c         -- normal exit.
          close(ioa)
          close(iob)
          write(lo,*)'stations written=',nswrt
          if(nssrt .eq. 0) then
              write(lo,*)'NO STATION/CASTS WERE SORTED' 
          else
              write(lo,*)'stations sorted=',nssrt
          endif
          stop
      endif
c
c -- get to here with more to do.
      line(1) = linsav
c     -- go up and decode the first line and read some more.
      go to 16
c
      end
