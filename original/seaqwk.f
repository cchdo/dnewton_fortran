      subroutine seaqwk(line,le,bg,en,stnnbr,castno,icd)
c
c -- routine to quickly uncode only certain parts of the WOCE format
c    .SEA line.  A return code is used to signal bad/blank lines.
c author: David Newton. Scripps Instn. of Oceanog.
c
      integer mxline
      parameter (mxline=300)
      character line*(mxline)
      integer mxnumo
      parameter (mxnumo=50)
      integer le,bg(-1:mxnumo),en(-1:mxnumo),icd,castno
      character stnnbr*6
c
c   line   -> .SEA line
c   le     -> fortran error unit.
c   bg     -> beginning columns (frm sealim)
c   en     -> ending columns (frm sealim)
c   stnnbr <- station number. (character) (left justified)
c   castno <- cast number 
c   icd    <-  return code. 0=okay, -1=blank or bogus line, 2=uncoding err.
c
      integer nc,i1
      character c*50
c
      external ljustn
c
      icd = 0
      stnnbr = ' '
      castno = -9
c
c -- station number.
      if(bg(1) .ne. 0) then
          c = line(bg(1):en(1))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              icd = -1
              return
          else if(nc .gt. 6) then
              write(le,*)'seaqwk: station num .gt. 6 chars.'
              icd =  2
              return
          else
              stnnbr = c(1:6)
          endif
      endif

c
c -- cast number.
      if(bg(2) .ne. 0) then
          c = line(bg(2):en(2))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,*)'seaqwk: castno is blank.'
              icd = 2
              return
          else
              if(nc .gt. 3) then
                  write(le,*)'seaqwk: castno too many chars. max=3'
                  icd = 2
                  return
              endif
              read(c(1:3),'(bn,i3)',iostat=icd) i1
              if(icd .ne. 0) then
                  write(le,2030) 2,bg(2),en(2)
 2030             format(' seaqwk: read err. vc=',i2,
     *                   ' cols ',i3,'->',i3)
                  icd = 2
                  return
              else
                  castno = i1
              endif
          endif
      endif
c
      return
      end
