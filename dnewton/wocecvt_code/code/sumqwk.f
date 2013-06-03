      subroutine sumqwk(line,le,bg,en,first,expo,stnnbr,
     *                  castno,ctype,tcode,icd)
c
c -- routine to quickly uncode only certain parts of the WOCE format
c    .SUM line. also checks that the expo code hasn't changed from the
c    time the routine was called with FIRST=.true. (use this feature
c    to detect blank or nonformatted lines)
c author: David Newton. Scripps Instn. of Oceanog.
c
      character line*200
      integer mxhdng
      parameter (mxhdng=19)
      integer le,bg(mxhdng),en(mxhdng),icd,castno
      character expo*14,stnnbr*6,ctype*3,tcode*2
      logical first
c
c  line   ->  .SUM line.
c  le     ->  fortran error output unit.
c  bg     ->  beginning columns for each field. (frm sumlim)
c  en     ->  ending columns for each field. (frm sumlim)
c  first  <-> passed in as true the first time routine called. returns false.
c  expo   <-  expocode (left justified)
c  stnnbr <-  stnnbr (character) (left justified)
c  castno <-  cast number
c  ctype  <-  cast type.
c  tcode  <-  time code.
c  icd    <-  return code. 0=okay, -1=blank or bogus line, 2=uncoding err.
c
      integer nc,i1
      character c*50
      character exposv*14
      save exposv
c
      external ljustn
c
      icd = 0
c
c -- expocode.
      expo = ' '
      if(bg(1) .ne. 0) then
          c = line(bg(1):en(1))
          call ljustn(c,nc)
          if(first .and. nc .eq. 0) then
              write(le,2020) 'expocode'
 2020         format(' sumqwk: missing ',a)
              icd = 2
          else if(nc .gt. 14) then
              write(le,2030) 'expocode',14
 2030         format(' sumqwk: ',a,' is too big. max=',i3)
              icd = 2
          else
              expo = c(1:14)
          endif
      endif
c
c -- check expocode against saved one (if not first time through)
      if(first) then
          exposv = expo
          first = .false.
      else
          if(expo(1:5) .ne. exposv(1:5)) then
              icd = -1
              return
          endif
      endif
c
c -- station number.
      stnnbr = ' '
      if(bg(3) .ne. 0) then
          c = line(bg(3):en(3))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,2020)'stnnbr'
              icd = 2
          else if(nc .gt. 6) then
              write(le,2030)'stnnbr',6
              icd = 2
          else
              stnnbr = c(1:6)
          endif
      endif
c
c -- cast number.
      castno = -9
      if(bg(4) .ne. 0) then
          c = line(bg(4):en(4))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,2020)'cast number'
              icd = 2
          else if(nc .gt. 3) then
              write(le,2030)'cast number',3
              icd = 2
          else
              read(c(1:3),'(bn,i3)',iostat=icd) i1
              if(icd .ne. 0) then
                  write(le,2040)'cast number',icd
 2040             format(' sumqwk: read err on ',a,' iostat=',i7)
                  icd = 2
              else
                  castno = i1
              endif
          endif
      endif
c
c -- cast type
      ctype = ' '
      if(bg(5) .ne. 0) then
          c = line(bg(5):en(5))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,2020)'cast type'
              icd = 2
          else if(nc .gt. 3) then
              write(le,2030)'cast type',3
              icd = 2
          else
              ctype = c(1:3)
          endif
      endif
c
c -- time code
      tcode = ' '
      if(bg(8) .ne. 0) then
          c = line(bg(8):en(8))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else if(nc .gt. 2) then
              write(le,2030)'time code',2
              icd = 2
          else
              tcode = c(1:2)
          endif
      endif
c
      return
      end
