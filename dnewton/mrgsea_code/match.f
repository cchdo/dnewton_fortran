      subroutine match(astn,istn,acastn,asamp,isamp,abotnm,ibotnm,
     *                 apress,estn,ecastn,esamp,ebotnm,epress,mmode,
     *                 nndl,prslim,idp)
c
c -- routine to find a possible match and return idp as it's position.
c
      integer istn(*),acastn(*),isamp(*),ibotnm(*),mmode,idp,
     *        ecastn,nndl
      character*8 astn(*),asamp(*),abotnm(*),estn,esamp,ebotnm
      real apress(*),epress,prslim
c
      integer i,icd,iestn,iesamp,iebotn
      logical intstn,intsmp,intbtl
      intrinsic abs
c
      intstn = .false.
      intsmp = .false.
      intbtl = .false.
c
      read(estn,2040,iostat=icd) iestn
 2040  format(i8)
      if(icd .ne. 0) then
          iestn = -9999
      else
          intstn = .true.
      endif
c
      if(mmode.eq.1 .or. mmode.eq.3) then
          read(esamp,2040,iostat=icd) iesamp
          if(icd .ne. 0) then
              iesamp = -9999
          else
              intsmp = .true.
          endif
      endif
      if(mmode .eq. 2) then
          read(ebotnm,2040,iostat=icd) iebotn
          if(icd .ne. 0) then
              iebotn = -9999
          else
              intbtl = .true.
          endif
      endif
c
      do 100 i=1,nndl
         if(intstn) then
c            -- check station number as an integer.
             if(istn(i) .ne. iestn) then
c                -- station doesn't match. go on.
                 go to 100
             endif
         else
c            -- check station number as alphanumeric.
             if(astn(i) .ne. estn) then
c                -- station doesn't match
                 go to 100
             endif
         endif
c
c        -- get to here with matching station. now look at other things.
         if(mmode.eq.1 .or. mmode.eq.2 .or. mmode.eq.4) then
c            -- check cast number. (integer)
             if(acastn(i) .ne. ecastn) then
c                -- cast doesn't match. skip out.
                 go to 100
             endif
         endif
c
         if(mmode.eq.1 .or. mmode.eq.3) then
c            -- check SAMPNO
             if(intsmp) then
c                -- check SAMPNO as an integer.
                 if(isamp(i) .eq. iesamp) then
c                    -- all criteria match.  return
                     idp = i
                     return
                 else
                     go to 100
                 endif
             else
c                -- check SAMPNO as an alphanumeric.
                 if(asamp(i) .eq. esamp) then
c                    -- all criteria match. return
                     idp = i
                     return
                 else
                     go to 100
                 endif
             endif
         endif
c
         if(mmode .eq. 2) then
c            -- check BTLNBR
             if(intbtl) then
c                -- check BTLNBR as an integer
                 if(ibotnm(i) .eq. iebotn) then
                     idp = i
                     return
                 else
                     go to 100
                 endif
             else
c                -- check BTLNBR as an alphanumeric
                 if(abotnm(i) .eq. ebotnm) then
                     idp = i
                     return
                 else
                     go to 100
                 endif
             endif
         endif
c
         if(mmode.eq.4 .or. mmode.eq.5) then
c            -- check CTDPRS
             if(abs(apress(i)-epress) .le. prslim) then
                 idp = i
                 return
             endif
         endif
 100  continue
c
c -- get here it nothing matches.
      idp = -1
      return
      end
