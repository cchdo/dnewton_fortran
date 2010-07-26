      subroutine cdsd2(line,le,bg,en,docart,ndat,grvlat,
     *                 c1sd2,c2sd2,icd)
c
c -- routine to code the two sd2 headers from a woce .SUM line.
c author: David Newton. Scripps Instn. of Oceanog.
c
c  line   -> woce .SUM line
c  le     -> fortran error unit.
c  bg     -> array of field beginnings (frm routine sumlim)
c  en     -> array of field endings (frm routine sumlim)
c  docart -> logical. true if you want to use carter depth correction for
c                     uncorrected depths.
c  ndat   -> number of observed depths. written to 2nd sd2 header.
c  grvlat <- returned latitude degree from .SUM line. used for gravity calc.
c  c1sd2  <- sd2 format header 1 card.
c  c2sd2  <- sd2 format header 2 card.
c  icd    <- return code.  0=okay  1=warning  2=fatal flaw.
c
      integer mxhdng
      parameter (mxhdng=19)
      integer le,bg(mxhdng),en(mxhdng),ndat,icd
      real grvlat
      character line*200,c1sd2*80,c2sd2*80
      logical docart
c
c changes:
c 12Jul93 dmn. added carter depth correction option.
c 19Jul93 dmn. added support for two bottom depths.
c 09Sep93 dmn. fixed carter depth. (wasn't using flat + flon)
c
      character ship*4,expo*14,whpid*5,stnnbr*6,ctype*3,tcode*2,
     *          lathem*1,lonhem*1,nav*3,params*20,comm*30
      integer castno,year,month,day,time,latdeg,londeg,botmun,
     *        botmco,hab,wheel,mxpres,nbots
      real latmin,lonmin
      logical warn
      real flat,flon
      integer carea,cdbotm
c
      integer th,tm
      character c2*2
c
      intrinsic nint,real,mod
      external sumun1,finar,dcorr
c
c -- call routine to uncode the .SUM line.
      call sumun1(line,le,bg,en,ship,expo,whpid,stnnbr,
     *             castno,ctype,year,month,day,time,tcode,
     *             latdeg,latmin,lathem,londeg,lonmin,lonhem,
     *             nav,botmun,botmco,hab,wheel,mxpres,nbots,
     *             params,comm,warn)
      if(warn) then
          write(le,*)'cdsd2: .SUM warning.'
      endif
c
      c1sd2 = ' '
      c2sd2 = ' '
c
      c1sd2(1:1) = '0'
c     -- country code.
      c1sd2(3:4) = ship(1:2)
      c1sd2(5:5) = '5'
c
      c2 = '19'
      if(ctype .eq. 'CTD') c2 = '62'
      c1sd2(14:15) = c2
c
      c1sd2(27:27) = lathem
      write(c1sd2(28:29),'(i2)') latdeg
      write(c1sd2(30:32),'(i3.2)') nint(latmin*10.)
      grvlat = real(latdeg)
      flat = real(latdeg) + latmin/60.
      if(lathem .eq. 'S') flat = -flat
c
      c1sd2(33:33) = lonhem
      write(c1sd2(34:36),'(i3)') londeg
      write(c1sd2(37:39),'(i3.2)') nint(lonmin*10.)
      flon = real(londeg) + lonmin/60.
      if(lonhem .eq. 'W') flon = -flon
c
      c1sd2(40:40) = '9'
      write(c1sd2(41:46),'(3i2.2)') year,month,day
      if(time .ne. -9) then
c         -- sd2 time is hours to 10ths. ugh.
          th = time/100
          tm = mod(time,100)
          tm = tm/6
          if(tm .gt. 9) tm = 9
          write(c1sd2(47:49),'(i2,i1)') th,tm
      endif
c
c     -- ship code.
      c1sd2(50:51) = ship(3:4)
c
      if(botmco .ne. -9) then
c         -- corrected depth available.
          write(c1sd2(56:60),'(i5)') botmco
      else if(docart .and. botmun .ne. -9) then
c         -- need to correct depth.
c         -- use carter table routines.  first find carter area.
          call finar(flat,flon,carea)
          if(carea .lt. 0) then
              write(le,*)'cdsd2: err rtn frm finar.flat,flon=',
     *                    flat,flon
          else
c             -- pass in uncorrected depth. get corrected out as cdbotm.
              call dcorr(botmun,carea,cdbotm)
              if(cdbotm .lt. 0) then
                  write(le,*)'cdsd2: err rtn frm dcorr.'
              else
                  write(c1sd2(56:60),'(i5)') cdbotm
              endif
          endif
      endif
c
      c1sd2(79:80) = '21'
c
c -- done with first record. now do second record.
c
c     -- put first 3 chars of whpid in cruise id.
      c2sd2(15:17) = whpid(1:3)
c
      c2sd2(18:23) = stnnbr
c
c     -- air temperatures missing.
      c2sd2(52:52) = '9'
      c2sd2(57:57) = '9'
c
c     -- write ndat as number of observations and detail records.
      write(c2sd2(62:64),'(i3)') ndat
      write(c2sd2(67:69),'(i3)') ndat
c
      c2sd2(79:80) = '32'
c
      return
      end
