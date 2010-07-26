      subroutine cddcfd(le,ioc,wbg,qualps,nobs,dat,qual,
     *                  qulmod,warn,icd)
c
c -- routine to code data values into DCF format. 
c -- written for wcvtcvt.   
c  author: David Newton, Scripps Instn. of Oceanog.  April 1994
c changes:
c   02May95 dmn added quality code conversion for woce ctd quality code 7
c               (7 = despiked)
c
      integer mxctd,mxcnum
      parameter (mxcnum=9,mxctd=8000)
      integer le,ioc,wbg(-1:mxcnum),nobs,qualps(-1:mxcnum),qulmod,icd
      real dat(mxctd,mxcnum)
      character qual(mxctd)*10
      logical warn
c
c le     -> fortran error unit
c ioc    -> fortran data output unit. already opened and used.
c wbg    -> array of beginning positions. from wctlim. used to figure
c          out what's available.
c qualps -> array of quality code positions. frm wctlim.
c nobs   -> number of pressure values. (levels)
c dat    -> array of data values from WCT format cast. from wctun2
c qual   -> array of quality flags from a WCT cast. frm wctun2
c           (note only one dimension in this subroutine)
c qulmod -> quality code conversion mode.
c           1=DQE mode, 2=picky mode, 3=Archive mode.
c warn   <- set to true if warning occurs.
c icd    <- error return code. non-zero is some sort of error.
c
      integer mxerr
      parameter (mxerr=12)
      integer i,j,k,iq,nerr,np,lstart
      character fmt*100,c1*1
      real tdata(9)
      integer pd(9),pq(9),q(9),qtab(0:9,3)
c
c     -- qtab translates WCT qual codes to DCF.
      data qtab/
     *          -1,1,1,1,1,0,1,1,-1,0,
     *          -1,1,1,0,0,0,2,2,-1,0,
     *          -1,1,1,3,0,0,2,2,-1,0/
c
      lstart = wbg(1)
      nerr = 0
      np = 0
      fmt = '('
      k = 2
      do 40 i=1,mxcnum
c        -- skip NUMBER.
         if(i .eq. 7) go to 40
         if(wbg(i) .ne. 0) then
             np = np + 1
             if(np .gt. 9) then
                 write(le,*)'cddcfd: prgrmr err. np too large.max=',9
                 stop 99
             endif
c            -- figure position in dat array.
             pd(np) = ((wbg(i)-lstart) / 8) + 1
c             --press
             if(i.eq.1) fmt(k:k+7) = 'f7.1,1x,'
c             --temperature
             if(i.eq.2) fmt(k:k+7) = 'f7.4,1x,'
c             --salinity
             if(i.eq.3) fmt(k:k+7) = 'f7.4,1x,'
c             --oxygen
             if(i.eq.4) fmt(k:k+7) = 'f7.1,1x,'
c             --xmiss
             if(i.eq.5) fmt(k:k+7) = 'f7.2,1x,'
c             --fluor
             if(i.eq.6) fmt(k:k+7) = 'f7.3,1x,'
c             --number (should never occur in wctcvt)
             if(i.eq.7) fmt(k:k+7) = 'f7.0,1x,'
c             --conductivity
             if(i.eq.8) fmt(k:k+7) = 'f7.4,1x,'
c             --sound
             if(i.eq.9) fmt(k:k+7) = 'f7.2,1x,'
             k = k + 8
             pq(np) = qualps(i)
         endif
 40   continue
      fmt(k:k+3) = '8i1)'
c
      do 80 i=1,nobs
         do 60 j=1,np
            tdata(j) = dat(i,pd(j))
c           -- translate quality codes.  (WCT scheme to DCF scheme)
            if(j .eq. 1) go to 60
            if(pq(j) .eq. 0) then
c              -- wct file had no quality code. assume okay.
               c1 = '2'
            else
               c1 = qual(i)(pq(j):pq(j))
            endif
            read(c1,'(i1)',iostat=icd) iq
            if(icd .ne. 0) then
                q(j) = -1
            else
                q(j) = qtab(iq,qulmod)
            endif
            if(iq .eq. 9 .and. tdata(j) .gt. -9.) then
                write(le,*)'cddcfd: flag shows missing, but value',
     *                     ' isn''t. press=',tdata(1)
                warn = .true.
            endif
            if(q(j) .gt. 0 .and. tdata(j) .le. -8.99999) then
                write(le,*)'cddcfd: value shows missing, but flag',
     *                     ' isn''t. press=',tdata(1)
                q(j) = 0
                warn = .true.
            endif
            if(q(j) .eq. -1) then
                q(j) = 0
                tdata(j) = -9.
                write(le,*)'cddcfd: bad qual flag = ',c1,'press=',
     *                      tdata(1)
                nerr = nerr + 1
                warn = .true.
            else if(q(j) .eq. 0) then
                tdata(j) = -9.
            endif
 60      continue
c
         write(ioc,fmt,iostat=icd) (tdata(j),j=1,np),(q(j),j=2,np)
         if(icd .ne. 0) then
             write(le,*)'cddcfd: write err=',icd,'fmt=',fmt,'press=',
     *                   tdata(1)
             warn = .true.
             return
         endif
c
         if(nerr .gt. mxerr) then
             write(le,*)'cddcfd: max errs reached. aborting this cast.'
             icd = 1
             warn = .true.
             return
         endif
 80   continue
c
      return
      end
