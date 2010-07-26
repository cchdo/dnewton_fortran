      subroutine cdieh(line,le,bg,en,docart,grvlat,comm,
     *                 c1ieh,c2ieh,icd)
c
c -- routine to code the two ieh headers from a woce .SUM line.
c author: David Newton. Scripps Instn. of Oceanog.
c
c  line   -> woce .SUM line
c  le     -> fortran error unit.
c  bg     -> array of field beginnings (frm routine sumlim)
c  en     -> array of field endings (frm routine sumlim)
c  docart -> logical. true if you want to use carter depth correction for
c                     uncorrected depths.
c  grvlat <- returned latitude degree from .SUM line. used for gravity calc.
c  comm   <- comment field from .SUM line. used as comment line in ieh format.
c  c1ieh  <- ieh format header 1 card.
c  c2ieh  <- ieh format header 2 card.
c  icd    <- return code.  0=okay  1=warning  2=fatal flaw.
c
      integer mxhdng
      parameter (mxhdng=19)
      integer le,bg(mxhdng),en(mxhdng),icd
      real grvlat
      character line*200,c1ieh*128,c2ieh*128
      logical docart
c
c changes:
c 12Jul93 dmn. added carter depth correction option.
c 19Jul93 dmn. added support for two bottom depths.
c 09Sep93 dmn. corrected args in call to sumun1.
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
      character c2*2
c
      intrinsic nint,real
      external sumun1,finar,dcorr
c
c -- call routine to uncode the .SUM line.
      call sumun1(line,le,bg,en,ship,expo,whpid,stnnbr,
     *             castno,ctype,year,month,day,time,tcode,
     *             latdeg,latmin,lathem,londeg,lonmin,lonhem,
     *             nav,botmun,botmco,hab,wheel,mxpres,nbots,
     *             params,comm,warn)
      if(warn) then
          write(le,*)'cdieh: .SUM warning.'
      endif
c
      c1ieh = ' '
      c2ieh = ' '
      write(c1ieh(1:2),'(i2)') latdeg
      write(c1ieh(3:5),'(i3.2)') nint(latmin*10.)
      c1ieh(6:6) = lathem
      grvlat = real(latdeg)
      flat = real(latdeg) + latmin/60.
      if(lathem .eq. 'S') flat = -flat
c
      write(c1ieh(7:9),'(i3)') londeg
      write(c1ieh(10:12),'(i3.2)') nint(lonmin*10.)
      c1ieh(13:13) = lonhem
      flon = real(londeg) + lonmin/60.
      if(lonhem .eq. 'W') flon = -flon
c
      write(c1ieh(14:19),'(3i2.2)') year,month,day
      if(time .ne. -9) then
          write(c1ieh(20:23),'(i4.4)') time
      endif
c
      if(botmco .ne. -9) then
c         -- corrected depth available.
          write(c1ieh(24:28),'(i5)') botmco
      else if(docart .and. botmun .ne. -9) then
c         -- need to correct depth.
c         -- use carter table routines.  first find carter area.
          call finar(flat,flon,carea)
          if(carea .lt. 0) then   
              write(le,*)'cdieh: err rtn frm finar.flat,flon=',
     *                    flat,flon
          else 
c             -- pass in uncorrected depth. get corrected out as cdbotm.
              call dcorr(botmun,carea,cdbotm)
              if(cdbotm .lt. 0) then
                  write(le,*)'cdieh: err rtn frm dcorr.'
              else
                  write(c1ieh(24:28),'(i5)') cdbotm
              endif
          endif
      endif
c
      c1ieh(56:57) = ship(1:2)
      c1ieh(60:61) = ship(3:4)
c
      c2 = 'HY'
      if(ctype .eq. 'CTD') c2 = 'CT'
      c1ieh(62:63) = c2
c
      c1ieh(64:65) = 'Z*'
c
c -- put cast number in processing number cols.
      if(castno .ne. -9) then
          write(c1ieh(66:68),'(i3)') castno
      endif
c
c -- put first 3 chars of whpid in cruise id.
      c1ieh(70:72) = whpid(1:3)
c
      c1ieh(75:80) = stnnbr
c
c -- put expocode(5:14) in first part of cruise name, whpid in 2nd part.
      c1ieh(89:98) = expo(5:14)
      c1ieh(99:103) = whpid
c
c -- wild columns are left blank for now.
c
      c1ieh(128:128) = '1'
c
c -- nothing goes on 2nd header now. latter could contain run time formats
c    for wild columns. and units too.
c
      c2ieh(64:65) = 'Z*'
      c2ieh(128:128) = '2'
c
      return
      end
c ****************************************************
      subroutine mkwlds(wldnam,wlddp,wldnum,units,wldfmt,
     *                  wldst1,wldst2)
c
c     -- routine to make wild column header strings for ieh format lines.
c
      character wldnam(3)*7,wldst1*24,wldst2*45
      character units(-1:*)*8,wldfmt(3)*6
      integer wlddp(3),wldnum(3)
c
      if(wldnum(1) .ne. -2) then
          wldst1(2:8) = wldnam(1)
          wldst2(1:1) = 'I'
          write(wldfmt(1),2010) wlddp(1)
 2010      format('(F7.',i1,')')
          wldst2(4:9) = wldfmt(1)
          wldst2(22:29) = units(wldnum(1))
      endif
c
      if(wldnum(2) .ne. -2) then
          wldst1(10:16) = wldnam(2)
          wldst2(2:2) = 'I'
          write(wldfmt(2),2010) wlddp(2)
          wldst2(10:15) = wldfmt(2)
          wldst2(30:37) = units(wldnum(2))
      endif
c
      if(wldnum(3) .ne. -2) then
          wldst1(18:24) = wldnam(3)
          wldst2(3:3) = 'I'
          write(wldfmt(3),2010) wlddp(3)
          wldst2(16:21) = wldfmt(3)
          wldst2(38:45) = units(wldnum(3))
      endif
c
      return
      end
