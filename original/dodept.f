      subroutine dodept(p,t,tq,s,sq,ndat,grvlat,z)
c
c -- routine to convert pressure to depth.
c    goes through too much work to find a crude dynamic depth.
c author: David Newton. Scripps Instn. of Oceanog.
c
      real p(*),t(*),s(*),grvlat,z(*)
      integer tq(*),sq(*),ndat
c
      integer mxdat
      parameter (mxdat=60)
      real sva(mxdat),dyn(mxdat)
      integer c,i,k
      real t1,t2,t3,s1,s2,s3,a,cdmiss,grv
      logical bad,approx
c
      real spvoly,grav
      external spvoly,grav,pzcon
c
      bad(c) = c.eq.9 .or. c.eq.8
c
      data cdmiss/3.2e4/
c
      approx = .false.
c
c     -- find the first good temp + sal.
      t1 = -10.0
      do 10 i=1,ndat
         if(bad(tq(i)) .or. bad(sq(i))) then
c            -- temp or sal bad.
         else
             t1 = t(i)
             s1 = s(i)
             go to 11
         endif
 10   continue
 11   if(t1 .lt. -9.0) then
c         -- no good temp and salinity found.
          approx = .true.
          go to 80
      endif
c
c -- find the last good temp and sal.
      t2 = -10.0
      do 20 i=ndat,1,-1
         if(bad(tq(i)) .or. bad(sq(i))) then
c            -- temp or sal bad.
         else
             t2 = t(i)
             s2 = s(i)
             go to 21
         endif
 20   continue
 21   if(t2 .lt. -9.0) then
c         -- no good temp and salinity found.
          approx = .true.
          go to 80
      endif
c
c -- now get an average temp and sal.
c
      t3 = (t1+t2) / 2.
      s3 = (s1+s2) / 2.
c
      if(t3.gt.50. .or. s3.gt.50.) then
          write(*,*)'dodept: prgrmr err. t3,s3=',t3,s3
          stop 99
      endif
c
c -- calculate specific volume anomaly for each level.
c
      do 30 i=1,ndat
         t1 = t(i)
         if(bad(tq(i))) t1 = t3
         s1 = s(i)
         if(bad(sq(i))) s1 = s3
c
         sva(i) = spvoly(s1,t1,p(i))
 30   continue
c
c
 80   continue
c
c -- calculate dynamic depth (0 at the top)
      if(approx) then
          do 90 i=1,ndat
             dyn(i) = p(i) * .0005
 90       continue
      else
          dyn(1) = p(1) * sva(1) * 1.0e-5
          k = 1
          do 100 i=2,ndat
             if(sva(i) .gt. 31000.) then
                 dyn(i) = cdmiss
             else
                 a = (p(i)-p(k)) * (sva(i)+sva(k)) * 5.0e-6
                 dyn(i) = a + dyn(k)
                 k = i
             endif
 100      continue
      endif
c
c -- finally calculate depth.
c
      grv = grav(grvlat)
      do 110 i=1,ndat
         call pzcon('D',grv,dyn(i),p(i),z(i))
 110  continue
c
      return
      end
