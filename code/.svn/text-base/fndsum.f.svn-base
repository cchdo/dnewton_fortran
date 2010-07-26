      subroutine fndsum(ioa,le,bg,en,stnwnt,cstwnt,line,expo,icd)
c
c -- routine to return the WOCE .SUM line that is appropriate for
c    the requested station and cast.
c author: David Newton. Scripps Instn. of Oceanog.
c
c  ioa    -> fortran unit number with .SUM file already opened.
c  le     -> fortran error output unit.
c  bg     -> starting columns. (frm sumlim)
c  en     -> ending columns. (frm sumlim)
c  stnwnt -> the station you want. 6 character. MUST BE left justified if
c            alphanumeric.
c  cstwnt -> the cast you want.
c  line   <- the .SUM line returned.
c  expo   <- expocode from the returned .SUM line.
c  icd    <- return code.  0=okay  1=no station found. 2=some error.
c                        3 = cast not found.
c
      integer mxhdng,mxindx,mxline
      logical strict
      parameter (mxhdng=19,mxindx=400,mxline=50,strict=.true.)
      integer ioa,le,bg(mxhdng),en(mxhdng),cstwnt,icd
      character stnwnt*6,line*200
c
      character ship*4,expo*14,whpid*5,stnnbr*6,ctype*3,tcode*2,
     *          lathem*1,lonhem*1,nav*3,params*20,comm*30
      integer castno,year,month,day,time,latdeg,londeg,botmun,
     *        botmco,hab,wheel,mxpres,nbots
      real latmin,lonmin
      logical warn
c
      character stnprv*6,lines(mxline)*200,c200*200,c8*8,
     *          chrdex(mxindx)*6,adum*1
      integer where(mxindx),intdex(mxindx),nindx,score(mxindx),
     *        i,j,nlines,ncline,i1,k,istwnt
      logical frst,frqwk,wrncst(mxline),alpha
      save frst,nindx,where,intdex,chrdex
c
      intrinsic index
      external sumqwk,sumun1
      data frst/.true./
c
c -- the first time this routine is called an index to where stations are
c    in the .SUM file will be created.
c
      if(frst) then
          frst = .false.
          frqwk = .true.
          stnprv = ' '
          rewind(ioa)
          do 4 i=1,4
 4        read(ioa,2010) c8
 2010      format(a)
          if(index(c8,'---') .eq. 0) then
              write(le,*)'fndsum: was expecting ''---'' on 4th line',
     *                   'of .SUM file.'
              write(le,*)'fndsum: got=',c8
              icd = 2
              return
          endif
          nindx = 0
          do 20 i=5,5000
             read(ioa,2010,iostat=icd) c200
             if(icd .ne. 0) then
                 if(icd .lt. 0) then
c                    -- hit eof. go to further processing.
                     where(nindx+1) = i
                     icd = 0
                     go to 22
                 else
                     write(le,*)'fndsum: read err in .SUM file.',
     *                          '20loop. iostat,i=',icd,i
                     icd = 2
                     return
                 endif
             endif
             call sumqwk(c200,le,bg,en,frqwk,expo,stnnbr,castno,
     *                   ctype,tcode,icd)
             if(icd .ne. 0) then
                 if(icd .lt. 0) go to 20
                 write(le,*)'fndsum: err rtn frm sumqwk.'
                 icd = 2
                 return
             endif
             if(strict) then
         call sumun1(c200,le,bg,en,ship,expo,whpid,stnnbr,
     *                castno,ctype,year,month,day,time,tcode,
     *                latdeg,latmin,lathem,londeg,lonmin,lonhem,
     *                nav,botmun,botmco,hab,wheel,mxpres,nbots,
     *                params,comm,warn)
                if(warn) then
                    write(le,*)'fndsum: .SUM warning. irec=',i
                endif
             endif
             if(stnnbr .ne. stnprv) then
                 stnprv = stnnbr
                 nindx = nindx + 1
                 if(nindx .gt. mxindx) then
                     write(le,*)'fndsum: too many stations to index.',
     *                          'max=',mxindx
                     icd = 2
                     return
                 endif
                 chrdex(nindx) = stnnbr
                 read(stnnbr,'(bn,i6)',iostat=icd) i1
                 if(icd .ne. 0) then
                     intdex(nindx) = -9999
                     icd = 0
                 else
                     intdex(nindx) = i1
                 endif
                 where(nindx) = i
             endif
 20       continue
          write(le,*)'fndsum: too many lines in .SUM file.'
          stop 99
      endif
c
c -- get to here with ready to examine indexes.
c
 22   continue
c     -- determine if stnwnt can be interpreted as an integer.
      read(stnwnt,'(bn,i6)',iostat=icd) istwnt
      if(icd .ne. 0) then
          alpha = .true.
          istwnt = -9999
      else
          alpha = .false.
      endif
      icd = 0
c
      do 30 i=1,nindx
         if(alpha) then
             if(stnwnt .eq. chrdex(i)) then
                 k = i
                 go to 32
             endif
         else if(intdex(i) .gt. -9998) then
             if(istwnt .eq. intdex(i)) then
                 k = i
                 go to 32
             endif
         endif
 30   continue
      write(le,*)'fndsum: no STNNBR=',stnwnt,' in .SUM file.'
      icd = 1
      return
c
 32   continue
c -- get here with K being a pointer to the index. where(k) is first line
c    that matches the requested station.
      if(k .le. 0 .or. k .gt. mxindx-1) then
          write(le,*)'fndsum: prgrmr err. aft 32. k=',k
          stop 99
      endif
c
c -- read the stuff before what we want.
c
      rewind(ioa)
      do 40 i=1,where(k)-1
         read(ioa,2010,iostat=icd) adum
         if(icd .ne. 0) then
             write(le,*)'fndsum: prgrmr err. 40 loop. iostat=',icd
             stop 99
         endif
 40   continue
c
c -- read the lines we want. (till one before the next station)
c
      nlines = 0
      do 45 i=where(k),where(k+1)-1
         nlines = nlines + 1
         if(nlines .gt. mxline) then
             write(le,*)'fndsum: too many lines of same station.',
     *                  ' max=',mxline,' stn=',stnwnt
             icd = 2
             return
         endif
         read(ioa,2010,iostat=icd) lines(nlines)
         if(icd .ne. 0) then
             write(le,*)'fndsum: prgrmr err. 45 loop. iostat=',icd
             stop 99
         endif
 45   continue
c
      if(nlines .le. 0) then
          write(le,*)'fndsum: prgrmr err. 0 lines in 45 loop.',
     *         'k,where(k),where(k+1)=',k,where(k),where(k+1)
          stop 99
      endif
c
c -- now pare down the saved lines to just the ones that have the right
c    cast.
c
      ncline = 0
      do 50 i=1,nlines
         call sumqwk(lines(i),le,bg,en,frqwk,expo,stnnbr,castno,
     *                   ctype,tcode,icd)
         if(icd .eq. -1) go to 50
         if(castno .eq. cstwnt) then
             ncline = ncline+1
             if(i .ne. ncline) lines(ncline) = lines(i)
         endif
 50   continue
c
      if(ncline .le. 0) then
          write(le,*)'fndsum: found stn=',stnwnt,'. can''t find cast=',       
     *               cstwnt
          icd = 3
          return
      endif
c
c -- take a shortcut it there's only one line with that cast.
c
      if(ncline .eq. 1) then
          call sumqwk(lines(1),le,bg,en,frqwk,expo,stnnbr,castno,
     *                   ctype,tcode,icd)
          line = lines(1)
          icd = 0
          return
      endif
c
c -- if there's more than one line for a cast. decide which one to use.
c    decode each one completely and see which one has the most stuff recorded.
c
      do 60 i=1,ncline
         call sumun1(lines(i),le,bg,en,ship,expo,whpid,stnnbr,
     *                castno,ctype,year,month,day,time,tcode,
     *                latdeg,latmin,lathem,londeg,lonmin,lonhem,
     *                nav,botmun,botmco,hab,wheel,mxpres,nbots,
     *                params,comm,warn)
         wrncst(i) = warn
         score(i) = 0
         if(botmun .gt. -8) score(i) = score(i) + 1
         if(botmco .gt. -8) score(i) = score(i) + 1
         if(hab .gt. -8) score(i) = score(i) + 1
         if(wheel .gt. -8) score(i) = score(i) + 1
         if(mxpres .gt. -8) score(i) = score(i) + 1
         if(nbots .gt. -8) score(i) = score(i) + 1
         if(params(1:1) .ne. ' ') score(i) = score(i) + 1
         if(comm(1:1) .ne. ' ') score(i) = score(i) + 1
 60   continue
c
      j = 1
      do 70 i=2,ncline
         if(score(i) .gt. score(j)) j = i
 70   continue
c
c -- "j" is the winner.
      if(wrncst(j)) then
          write(le,*)'fndsum: CAUTION. stn=',stnwnt,' cst=',cstwnt
          write(le,*)'produced an error on the .SUM line used.'
      endif
c
      line = lines(j)
c
      return
      end
