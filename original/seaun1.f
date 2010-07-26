      subroutine seaun1(line,le,bg,en,qualps,nast,stnnbr,castno,
     *                  dat,qual,warn)
c
c -- routine to uncode woce format .SEA line. 
c    requires field limits from subroutine sealim.
c author: David Newton. Scripps Instn. of Oceanog.
c
      integer mxnumo,mxline
      parameter (mxnumo=50,mxline=300)
      integer le,bg(-1:mxnumo),en(-1:mxnumo),qualps(-1:mxnumo),
     *        nast,castno
      real dat(5:mxnumo)
      character line*(mxline),stnnbr*6,qual(1:mxnumo,2)*1
      logical warn
c
c line   -> woce data line from .SEA file.
c le     -> fortran error output unit.
c bg     -> array with starting columns of .SEA file. (frm sealim)
c en     -> array with ending columns of .SEA file. (frm sealim)
c qualps -> array with position of quality code for each parameter. (frm sealim)
c nast   -> number of asterisked values. (frm sealim)
c stnnbr <- station number from data line (character). (left justified)
c castno <- cast number from data line. (integer)
c dat    <- data read off line. (indecies are same as routine sealim)
c qual   <- quality codes for each read parameter.
c warn   <- returns as true if something went weird. false otherwise.
c
      integer mxnast
      parameter (mxnast=25)
      integer j,k,i1,nc,icd
      real f1
      character c*(mxnast),c8*8,q1str*(mxnast),q2str*(mxnast)
c
      external ljustn
c
      warn = .false.
c
      if(nast .gt. mxnast) then
          write(le,*)'seaun1: too many quality bytes. max=',mxnast
          write(le,*)'seaun1: have prgrmr increase mxnast.'
          stop 99
      endif
c
c -- extract quality flag fields.
      q1str = ' '
      if(bg(-1) .eq. 0) then
          write(le,*)'seaun1: no QUALT1 keyword. no data decoding.'
          stop 99
      else
          c = line(bg(-1):en(-1))
          call ljustn(c,nc)
          if(nc .ne. nast) then
              write(le,*)'seaun1: wrong number of QUALT1 flags.'
              write(le,*)' from asterisk count,from data =',
     *                   nast,nc
              warn = .true.
          else
              q1str = c(1:nc)
          endif
      endif
c
      q2str = ' '
      if(bg(0) .ne. 0) then
          c = line(bg(0):en(0))
          call ljustn(c,nc)
          if(nc .ne. nast) then
              write(le,*)'seaun1: wrong number of QUALT2 flags.'
              write(le,*)' from asterisk count,from data =',
     *                   nast,nc
              warn = .true.
          else
              q2str = c(1:nc)
          endif
      endif
c
c -- station number.
      stnnbr = ' '
      qual(1,1) = ' '
      qual(1,2) = ' '
      if(bg(1) .ne. 0) then
          c = line(bg(1):en(1))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,*)'seaun1: station num is blank.'
              warn = .true.
          else if(nc .gt. 6) then
              write(le,*)'seaun1: station num .gt. 6 chars.'
              warn = .true.
          else
              stnnbr = c(1:6)
          endif
      endif
c
c -- cast number.
      castno = -9
      if(bg(2) .ne. 0) then
          c = line(bg(2):en(2))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,*)'seaun1: castno is blank.'
              warn = .true.
          else
              if(nc .gt. 3) then
                  write(le,*)'seaun1: castno too many chars. max=3'
                  warn = .true.
              endif
              read(c(1:3),'(bn,i3)',iostat=icd) i1
              if(icd .ne. 0) then
                  write(le,2030) 2,bg(2),en(2)
 2030             format(' seaun1: read err. vc=',i2,
     *                   ' cols ',i3,'->',i3)
                  warn = .true.
              else
                  castno = i1
              endif
          endif
      endif
c
c -- finally decode all the rest of the real data.
c
      do 60 j=5,mxnumo
         dat(j) = 0.0
         qual(j,1) = ' '
         qual(j,2) = ' '
         if(bg(j) .ne. 0) then
             c8 = line(bg(j):en(j))
             call ljustn(c8,nc)
             if(nc .eq. 0) then
                 write(le,2040) j,bg(j),en(j)
 2040             format(' seaun1: blank field. vc=',i2,
     *                   ' cols ',i3,'->',i3)
                 warn = .true.
             else
                 read(c8,'(bn,f8.0)',iostat=icd) f1
                 if(icd .ne. 0) then
                     write(le,2030) j,bg(j),en(j)
                     warn = .true.
                 else
                     dat(j) = f1
                 endif
             endif
c            -- grab the quality byte(s).
             if(qualps(j) .ne. 0) then
                 k = qualps(j)
                 qual(j,1) = q1str(k:k)
                 qual(j,2) = q2str(k:k)
             endif
         endif
 60   continue
c
      return
      end
