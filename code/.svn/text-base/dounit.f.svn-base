      subroutine dounit(bg,units,p,t,tq,s,sq,ox,oxq,sil,silq,
     *                  no3,no3q,no2,no2q,po4,po4q,ndat)
c
c -- routine to convert typical hydrographic parameters (oxygen + nutrients)
c  from woce (SI) (/kilogram) units to historical (/liter) units.
c author: David Newton. Scripps Instn. of Oceanog.
c
      real p(*),t(*),s(*),ox(*),sil(*),no3(*),no2(*),po4(*)
      integer bg(-1:*),tq(*),sq(*),oxq(*),silq(*),no3q(*),no2q(*),
     *        po4q(*),ndat
      character units(-1:*)*8
c
      logical cvox,cvsil,cvno3,cvno2,cvpo4
      real sigth,stmp,v,pden
      integer i
c
      intrinsic index
      external sigmp
c
c     -- check to see if oxygen must be converted.
      if(bg(12) .eq. 0) then
          cvox = .false.
      else if(index(units(12),'/L') .ne. 0) then
c         -- found '/L' in units. assume data already in historical units.
          cvox = .false.
      else
          cvox = .true.
      endif
c
c     -- check to see if silicate must be converted.
      if(bg(13) .eq. 0) then
          cvsil = .false.
      else if(index(units(13),'/L') .ne. 0) then
          cvsil = .false.
      else
          cvsil = .true.
      endif
c
c     -- check to see if no3(nitrate) must be converted.
      if(bg(14) .eq. 0) then
          cvno3 = .false.
      else if(index(units(14),'/L') .ne. 0) then
          cvno3 = .false.
      else
          cvno3 = .true.
      endif
c
c     -- check to see if no2(nitrite) must be converted.
      if(bg(15) .eq. 0) then
          cvno2 = .false.
      else if(index(units(15),'/L') .ne. 0) then
          cvno2 = .false.
      else
          cvno2 = .true.
      endif
c
c     -- check to see if po4(phosphate) must be converted.
      if(bg(16) .eq. 0) then
          cvpo4 = .false.
      else if(index(units(16),'/L') .ne. 0) then
          cvpo4 = .false.
      else
          cvpo4 = .true.
      endif
c
c -- see if anything needs to be done. (probably)
      if(cvox .or. cvsil .or. cvno3 .or. cvno2 .or. cvpo4) then
c         -- keep going.
      else
c         -- everything's in historical units.
          return
      endif
c
      do 20 i=1,ndat
c        -- store a salinity for use in density calculations. defaults to 34.8
         stmp = 34.8
         if(sq(i) .ne. 9) stmp = s(i)
c
         if(cvox) then
c            -- oxygen needs to be converted.
             if(oxq(i) .ne. 9) then
                 if(tq(i) .eq. 9) then
c                    -- missing temperature. make approximation.
                     v = ox(i) / 43.5
                 else
                     call sigmp(0.0,p(i),t(i),stmp,sigth)
                     v = ox(i) * .022392 * (sigth/1000. + 1.0)
                 endif
                 ox(i) = v
             endif
         endif
c
c        -- calculate a potential density that is used for the nutrients.
         call sigmp(0.0,0.0,25.0,stmp,pden)
         v = pden/1000. + 1.0
c
         if(cvsil .and. silq(i).ne.9) sil(i) = sil(i) * v
         if(cvno3 .and. no3q(i).ne.9) no3(i) = no3(i) * v
         if(cvno2 .and. no2q(i).ne.9) no2(i) = no2(i) * v
         if(cvpo4 .and. po4q(i).ne.9) po4(i) = po4(i) * v
 20   continue
c
      return
      end
