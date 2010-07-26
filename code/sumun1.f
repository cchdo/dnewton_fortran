      subroutine sumun1(line,le,bg,en,ship,expo,whpid,stnnbr,
     *                castno,ctype,year,month,day,time,tcode,
     *                latdeg,latmin,lathem,londeg,lonmin,lonhem,
     *                nav,botmun,botmco,hab,wheel,mxpres,nbots,
     *                params,comm,warn)
c
c -- routine to uncode a WOCE .SUM file line given column limits.
c     from subroutine sumlim.  Extensive error trapping.
c author: David Newton. Scripps Instn. of Oceanog.
c
      character line*200
      integer mxhdng
      parameter (mxhdng=19)
      integer le,bg(mxhdng),en(mxhdng)
      character ship*4,expo*14,whpid*5,stnnbr*6,ctype*3,tcode*2,
     *          lathem*1,lonhem*1,nav*3,params*20,comm*30
      integer castno,year,month,day,time,latdeg,londeg,botmun,
     *        botmco,hab,wheel,mxpres,nbots
      real latmin,lonmin
      logical warn 
c
c  line   ->  .SUM line.
c  le     ->  fortran error output unit.
c  bg     ->  beginning columns for each field. (frm sumlim)
c  en     ->  ending columns for each field. (frm sumlim)
c  ship   <-  first 4 characters of expocode.
c  expo   <-  expocode (left justified)
c  whpid  <-  section number. left justified.
c  stnnbr <-  stnnbr (character) (left justified)
c  castno <-  cast number
c  ctype  <-  cast type.
c  year   <-  2 digit integer year.
c  month  <-  2 digit integer month.
c  day    <-  2 digit integer day.
c  time   <-  4 digit integer time HHMM.
c  tcode  <-  2 character time code.
c  latdeg <-  latitude degrees. 0-90
c  latmin <-  latitude minutes.
c  lathem <-  'N' or 'S'
c  londeg <-  longitude degrees. 0-180
c  lonmin <-  longitude minutes.
c  lonhem <-  'E' or 'W'
c  nav    <-  navigation type code. 3 char left justified.
c  botmun <-  depth to bottom uncorrected 
c  botmco <-  depth to bottom corrected 
c  hab    <-  height above bottom
c  wheel  <-  meter wheel reading.
c  mxpres <-  maximum pressure.
c  nbots  <-  numbers of bottles indicated on .SUM line. (unreliable)
c  params <-  string of parameter codes. left justified. (unreliable) 20 char mx
c  comm   <-  comment. 30 char max. left justified.
c  warn   <-  returns as true if a problem was detected. 
c
c  missing character vars return as blank. missing integer values return -9
c  missing reals (latmin,lonmin) return -9.
c
c changes:
c  30Jun93 dmn. added time codes 'RE' and 'UN'
c  19Jul93 dmn. replaced botm with botmun and botmco. (uncorrected + corrected
c               bottom depth)
c
      real f1
      integer i,i1,i2,i3,nc,icd
      character c*50,fmt*40,spec*12,a1*1
      logical embed
c
      intrinsic nint
      external ljustn,rtnfmt
c
c
      warn = .false.
c -- expocode.
      expo = ' '
      ship = ' '
      if(bg(1) .ne. 0) then
          c = line(bg(1):en(1))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,2020) 'expocode'
 2020         format(' sumun1: missing ',a)
              warn = .true.
          else if(nc .gt. 14) then
              write(le,2030) 'expocode',14
 2030         format(' sumun1: ',a,' is too big. max=',i3)
              warn = .true.
          else
              expo = c(1:14)
              ship = expo(1:4)
          endif
      endif
c
c -- whpid (section)
      whpid = ' '
      if(bg(2) .ne. 0) then
          c = line(bg(2):en(2))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else if(nc .gt. 5) then
              write(le,2030) 'whpid (section)',5
              warn = .true.
          else
              whpid = c(1:5)
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
              warn = .true.
          else if(nc .gt. 6) then
              write(le,2030)'stnnbr',6
              warn = .true.
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
              warn = .true.
          else if(nc .gt. 3) then
              write(le,2030)'cast number',3
              warn = .true.
          else
              read(c(1:3),'(bn,i3)',iostat=icd) i1
              if(icd .ne. 0) then
                  write(le,2040)'cast number',icd
 2040             format(' sumun1: read err on ',a,' iostat=',i7)
                  warn = .true.
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
              warn = .true.
          else if(nc .gt. 3) then
              write(le,2030)'cast type',3
              warn = .true.
          else
              ctype = c(1:3)
          endif
      endif
c
c -- date.
      year = -9
      month = -9
      day = -9
      if(bg(6) .ne. 0) then
          c = line(bg(6):en(6))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,2020)'date'
              warn = .true.
          else
              embed = .false.
              do 40 i1=2,nc-1
                 if(c(i1:i1) .eq. ' ') embed = .true.
 40           continue
              if(embed) then
                write(le,*)' sumun1: embedded blanks in date.'
                warn = .true.
              endif
              if(nc .eq. 6) then
                read(c(1:6),'(bz,3i2)',iostat=icd) i1,i2,i3
              else
                write(le,*)'sumun1: date string should be 6 digits.'
                warn = .true.
                write(fmt,2060) nc
 2060           format('(BZ,I',i2,')')
                read(c(1:nc),fmt,iostat=icd) i1
                if(icd .ne. 0) then
                  write(le,2040)'date',icd
                else
                  write(c(1:6),'(i6.6)') i1
                  read(c(1:6),'(bz,3i2)',iostat=icd)i1,i2,i3
                endif
              endif
              if(icd .ne. 0) then
                write(le,2040)'date',icd
                warn = .true.
              else  
                if(i1.gt.12 .or. i2.gt.31) then
                    write(le,*)' sumun1: month or day bad=',i1,i2
                    warn = .true.
                else
                    month = i1
                    day = i2
                endif
                year = i3
              endif
          endif 
      endif
c
c -- time
      time = -9 
      if(bg(7) .ne. 0) then
          c = line(bg(7):en(7))
          call ljustn(c,nc)
          if(nc .eq. 0) then
c             -- nothing.
          else
              embed = .false.
              do 50 i=2,nc-1
                 if(c(i:i) .eq. ' ') embed = .true.
 50           continue
              if(embed) then
                write(le,*)' sumun1: embedded blanks in time.'
                warn = .true.
              endif
              write(fmt,2060) nc
              read(c(1:nc),fmt,iostat=icd) i3
              if(icd .ne. 0) then
                  write(le,2040)'time',icd
                  warn = .true.
              else if(i3 .eq. -9) then
              else
                  write(c(1:4),'(i4.4)') i3
                  read(c(1:4),'(2i2)',iostat=icd) i1,i2
              endif
              if(icd .ne. 0) then
                  write(le,2040)'time',icd
                  warn = .true.
              else if(i3 .eq. -9) then
                  time = i3
              else
                  if(i1.gt.24 .or. i2.gt.59) then
                      write(le,*)' sumun1: hour or minute bad=',i1,i2
                      warn = .true.
                  else if(i1.eq.24 .and. i2.ne.0) then
                      write(le,*)' sumun1: hour or minute bad=',i1,i2
                      warn = .true.
                  else
                      time = i3
                  endif
              endif
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
              warn = .true.
          else
              tcode = c(1:2)
          endif
          if(tcode.eq.'AT'.or.tcode.eq.'BE'.or.tcode.eq.'BO'.or.
     *       tcode.eq.'DE'.or.tcode.eq.'EN'.or.tcode.eq.'MR'.or.
     *       tcode.eq.'RE'.or.tcode.eq.'UN'.or.
     *       tcode.eq.'  ') then
c             -- time code is recognized.
          else
              write(le,*)'sumun1: time code not recognized=',tcode
              warn = .true.
          endif
      endif
c
c -- latitude.
      latdeg = -9
      latmin = -9.
      lathem = 'U'
      if(bg(9) .ne. 0) then
          c = line(bg(9):en(9))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,2020)'latitude'
              warn = .true.
          else
              spec = '(IFA)'
              call rtnfmt(c,spec,fmt,*60)
              read(c,fmt,iostat=icd) i1,f1,a1
              if(icd .ne. 0) then
                  write(le,2040)'latitude',icd
                  warn = .true.
              else 
                  if(a1.eq.'N' .or. a1.eq.'S') then
                      lathem = a1
                  else
                      write(le,*)' sumun1: bad lathem=',a1
                      warn = .true.
                  endif
                  if(i1 .lt. 0 .or. i1 .gt. 90) then
                      write(le,*)' sumun1: bad latdeg=',i1
                      warn = .true.
                  else
                      latdeg = i1
                  endif
                  if(f1 .lt. 0.0 .or. f1 .ge. 60.0) then
                      write(le,*)' sumun1: bad latmin=',f1
                      warn = .true.
                  else
                      latmin = f1
                  endif
              endif
              go to 62
c                -- get to here on err rtn from rtnfmt.
 60              write(le,2030)' sumun1: err rtn from rtnfmt. spec=',
     *                         spec,' uncoding latitude string'
                 warn = .true.
 62           continue
          endif
      endif
c
c -- longitude.
      londeg = -9
      lonmin = -9.
      lonhem = 'U'
      if(bg(10) .ne. 0) then
          c = line(bg(10):en(10))
          call ljustn(c,nc)
          if(nc .eq. 0) then
              write(le,2020)'longitude'
              warn = .true.
          else
              spec = '(IFA)'
              call rtnfmt(c,spec,fmt,*70)
              read(c,fmt,iostat=icd) i1,f1,a1
              if(icd .ne. 0) then
                  write(le,2040)'longitude',icd
                  warn = .true.
              else 
                  if(a1.eq.'E' .or. a1.eq.'W') then
                      lonhem = a1
                  else
                      write(le,*)' sumun1: bad lonhem=',a1
                      warn = .true.
                  endif
                  if(i1 .lt. 0 .or. i1 .gt. 180) then
                      write(le,*)' sumun1: bad londeg=',i1
                      warn = .true.
                  else
                      londeg = i1
                  endif
                  if(f1 .lt. 0.0 .or. f1 .ge. 60.0) then
                      write(le,*)' sumun1: bad lonmin=',f1
                      warn = .true.
                  else
                      lonmin = f1
                  endif
              endif
              go to 72
c                -- get to here on err rtn from rtnfmt.
 70              write(le,*)' sumun1: err rtn from rtnfmt. spec=',
     *                         spec,' uncoding longitude string'
                 warn = .true.
 72           continue
          endif
      endif
c
c -- nav code.
      nav = ' '
      if(bg(11) .ne. 0) then
          c = line(bg(11):en(11))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else if(nc .gt. 3) then
              write(le,2030) 'nav code',3
              warn = .true.
          else
              nav = c(1:3)
          endif
      endif
c
c -- bottom depth. uncorrected.
      botmun = -9
      if(bg(12) .ne. 0) then
          c = line(bg(12):en(12))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else 
              write(fmt,2070) nc
 2070         format('(BN,F',i2,'.0)')
              read(c(1:nc),fmt,iostat=icd) f1
              if(icd .ne. 0) then
                  write(le,2040)'bottom depth (unc)',icd
                  warn = .true.
              else if(f1 .le. 0.1) then
c                 -- zero bottom depth has to mean missing.
              else
                  botmun = nint(f1)
              endif
          endif
      endif
c
c -- bottom depth. corrected.
      botmco = -9
      if(bg(19) .ne. 0) then
          c = line(bg(19):en(19))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else 
              write(fmt,2070) nc
              read(c(1:nc),fmt,iostat=icd) f1
              if(icd .ne. 0) then
                  write(le,2040)'bottom depth (cor)',icd
                  warn = .true.
              else if(f1 .le. 0.1) then
c                 -- zero bottom depth has to mean missing.
              else
                  botmco = nint(f1)
              endif
          endif
      endif
c
c -- height above bottom.
      hab = -9
      if(bg(13) .ne. 0) then
          c = line(bg(13):en(13))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else
              write(fmt,2070) nc
              read(c(1:nc),fmt,iostat=icd) f1
              if(icd .ne. 0) then
                  write(le,2040)'height above bottom',icd
                  warn = .true.
              else
                  hab = nint(f1)
              endif
          endif
      endif
c
c -- meter wheel.
      wheel = -9
      if(bg(14) .ne. 0) then
          c = line(bg(14):en(14))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else
              write(fmt,2070) nc
              read(c(1:nc),fmt,iostat=icd) f1
              if(icd .ne. 0) then
                  write(le,2040)'meter wheel',icd
                  warn = .true.
              else
                  wheel = nint(f1)
              endif
          endif
      endif
c
c -- maximum pressure.
      mxpres = -9
      if(bg(15) .ne. 0) then
          c = line(bg(15):en(15))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else
              write(fmt,2070) nc
              read(c(1:nc),fmt,iostat=icd) f1
              if(icd .ne. 0) then
                  write(le,2040)'max pressure',icd
                  warn = .true.
              else
                  mxpres = nint(f1)
              endif
          endif
      endif
c
c -- number of bottles.
      nbots = -9
      if(bg(16) .ne. 0) then
          c = line(bg(16):en(16))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else
              write(fmt,2070) nc
              read(c(1:nc),fmt,iostat=icd) f1
              if(icd .ne. 0) then
                  write(le,2040)'number of bottles',icd
                  warn = .true.
              else
                  nbots = nint(f1)
              endif
          endif
      endif
c
c -- parameters collected.
      params = ' '
      if(bg(17) .ne. 0) then
          c = line(bg(17):en(17))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else 
              if(nc .gt. 20) then
                  write(le,*)' sumun1: parameter list truncated.'
              endif
              params = c(1:nc)
          endif
      endif
c
c -- comment.
      comm = ' '
      if(bg(18) .ne. 0) then
          c = line(bg(18):en(18))
          call ljustn(c,nc)
          if(nc .eq. 0) then
          else 
              if(nc .gt. 30) then
                  write(le,*)' sumun1: comment truncated.'
              endif
              comm = c(1:nc)
          endif
      endif
c
c -- end of value decoding.
      return
      end
