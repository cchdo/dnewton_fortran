      subroutine wctun2(iob,le,bg,en,nast,nparam,nobs,dat,qual,
     *                  warn,icd)
c
c -- routine to read the data records in a WOCE CTD format (*.WCT) file.
c author: David Newton. Scripps Instn. of Oceanog. Apr94
c
      integer mxcnum,mxctd
      parameter (mxcnum=9,mxctd=8000)
      integer iob,le,bg(-1:mxcnum),en(-1:mxcnum),
     *        nast,nparam,nobs,icd
      real dat(mxctd,mxcnum)
      character qual(mxctd,2)*10
      logical warn
c
c le     ->  fortran error unit.
c iob    ->  fortran unit for .WCT file. already opened. will expect to read
c             4th line of file.
c bg     -> integer array filled with start positions of .WCT info.
c            indecies in data statement below.
c en     -> same as bg for ending column of info.
c nast   -> number of parameters that had asterisks under them
c nparam -> number of data values (all except QUALTn) on the WCT line
c nobs   -> number of data records. read from WCT header.
c dat    <- real array of data as they appear in the WCT file.
c qual   <- char array of quality codes as they appear in the WCT file.
c warn   <- set to true on warning.
c icd    <- return code. 0=okay  1=some fatal error  non-zero=read error.
c
      integer irec,i,j,nsig,q1b,q1e,q2b,q2e
      character line*120,scrtch*20
c
      external ljustn
c
      irec = 6
      i = 0
 20   continue
      irec = irec + 1
      read(iob,2010,iostat=icd) line
 2010  format(a)
      if(icd .ne. 0) then
          write(le,*)'wctun2: read err in data. iostat,irec=',icd,irec
          if(icd .lt. 0) write(le,*)'     was expecting to read',
     *                   nobs,' data records.'
          return
      endif
      i = i + 1
      if(i .eq. 1) then
c         -- on first data record check # of quality flags and calculate
c            constants.
          q1b = 0
          q1e = 0
          q2b = 0
          q2e = 0
          if(bg(-1) .ne. 0) then
              scrtch = line(bg(-1):en(-1))
              call ljustn(scrtch,nsig)
              if(nast .ne. nsig) then
                  write(le,*)'wctun2: number of expected and observed',
     *              ' QUALT1 flags don''t match.'
                  write(le,*)'frm ast count=',nast,' observed=',nsig
                  icd = 1
                  return
              endif
              q1b = en(-1)-nast+1
              q1e = en(-1)
          endif
          if(bg(0) .ne. 0) then
              scrtch = line(bg(0):en(0))
              call ljustn(scrtch,nsig)
              if(nast .ne. nsig) then
                  write(le,*)'wctun2: number of expected and observed',
     *              ' QUALT2 flags don''t match.'
                  write(le,*)'frm ast count=',nast,' observed=',nsig
                  icd = 1
                  return
              endif
              q2b = en(0)-nast+1
              q2e = en(0)
          endif
      endif
c
      if(q1b .gt. 0) qual(i,1) = line(q1b:q1e)
      if(q2b .gt. 0) qual(i,2) = line(q2b:q2e)
c
      read(line,'(10f8.0)',iostat=icd) (dat(i,j),j=1,nparam)
      if(icd .ne. 0) then
          write(le,*)'wctun2: uncode err in data. iostat,irec=',icd,
     *                irec
          return
      endif
c
c -- normal return
      if(i .ge. nobs)  then
c         -- test to see if next read yields eof.
          read(iob,2010,iostat=icd) line
          if(icd .ge. 0) then
              write(le,*)'wctun2: was expecting end-of-file, but it',
     *                   ' didn''t occur. number of records correct?'
              warn = .true.
              return
          endif
          icd = 0
          return
      endif
c
c -- loop up for the next level.
      go to 20
      end
