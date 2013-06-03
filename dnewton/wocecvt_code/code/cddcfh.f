      subroutine cddcfh(line,le,bg,en,docart,cyear,cmonth,cday,
     *                 nobs,ocntry,instru,
     *                 h1,h2,h3,h4,h5)
c
c -- routine to code 5 DCF headers from a woce .SUM line.
c author: David Newton. Scripps Instn. of Oceanog.
c
c  line   -> woce .SUM line
c  le     -> fortran error unit.
c  bg     -> array of field beginnings (frm routine sumlim)
c  en     -> array of field endings (frm routine sumlim)
c  docart -> logical. true if you want to use carter depth correction for
c                     uncorrected depths.
c  cyear  -> year from .wct file. compared only.
c  cmonth -> month from .wct file. compared only.
c  cday   -> day from .wct file. compared only.
c  nobs   -> number of ctd levels.
c  ocntry -> 2 char originators country. If blank will use ship country.
c  instru -> 4 char instrument id from .WCT file.
c  h1,h2,h3,h4,h5 <- 1st 5 DCF format header lines. They need to be
c                    further modified outside this routine.
c                    (add NOBS, CC, MINPRS, MAXPRS)
c
      integer mxhdng
      parameter (mxhdng=19)
      integer le,bg(mxhdng),en(mxhdng)
      integer cyear,cmonth,cday,nobs
      character line*200,h1*37,h2*40,h3*39,h4*41,h5*41,ocntry*2,
     *          instru*4
      logical docart
c
c changes:
c 22Apr94 dmn first written
c
      character ship*4,expo*14,whpid*5,stnnbr*6,ctype*3,tcode*2,
     *          lathem*1,lonhem*1,nav*3,params*20,comm*30
      integer i,x,castno,year,month,day,time,latdeg,londeg,botmun,
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
          write(le,*)'cddcfh: .SUM warning.'
      endif
c
      h1 = ' '
      h2 = ' '
      h3 = ' '
      h4 = ' '
      h5 = ' '
      flat = real(latdeg) + latmin/60.
      if(lathem .eq. 'S') flat = -flat
c
      flon = real(londeg) + lonmin/60.
      if(lonhem .eq. 'W') flon = -flon
c
      write(h1,2031) flat,flon,nobs
 2031  format('LAT',1x,f8.4,1x,'LON',1x,f9.4,1x,'NOBS',1x,i5)
c
c -- compare date between .WCT file and .SUM file. trust .SUM file.
      if(year.ne.cyear .or. month.ne.cmonth .or. day.ne.cday) then
          write(le,*)'cddcfh: YMD dates don''t match. .SUM has=',year,
     *                month,day,' .WCT has=',cyear,cmonth,cday
          write(le,*)'stnnbr/castno= ',stnnbr,'/',castno
      endif
      i = year
      if(i .lt. 60) then
          i = i + 2000
      else
          i = i + 1900
      endif
      c2 = ocntry
      if(c2 .eq. '  ') c2 = ship(1:2)
      write(h2,2032) i,month,day,time,ship,c2
 2032  format('YMD',1x,i4,1x,i2,1x,i2,1x,'TIME',1x,i4.3,1x,'SHIP',
     *        1x,a4,1x,'CC',1x,a2) 
c
      x = -9
      if(botmco .ne. -9) then
c         -- corrected depth available.
          x = botmco
      else if(docart .and. botmun .ne. -9) then
c         -- need to correct depth.
c         -- use carter table routines.  first find carter area.
          call finar(flat,flon,carea)
          if(carea .lt. 0) then   
              write(le,*)'cddcfh: err rtn frm finar.flat,flon=',
     *                    flat,flon
          else 
c             -- pass in uncorrected depth. get corrected out as cdbotm.
              call dcorr(botmun,carea,cdbotm)
              if(cdbotm .lt. 0) then
                  write(le,*)'cddcfh: err rtn frm dcorr.'
              else
                  x = cdbotm
              endif
          endif
      endif
      write(h3,2033) expo(5:14),whpid,x
 2033  format('CRUISE',1x,a10,1x,'SECT',1x,a5,1x,'BOTM',1x,i5)
c
      write(h4,2034) stnnbr,castno,-9.
 2034  format('STATION',1x,a6,4x,1x,'CAST',1x,i2,1x,'MINPRS',f7.1)
c
      write(h5,2035) instru,'-',-9.
 2035  format('INSTRUMENT',1x,a4,2x,1x,'CSTDIR',1x,a1,1x,'MAXPRS',1x,
     *        f7.1)
c
      return
      end
