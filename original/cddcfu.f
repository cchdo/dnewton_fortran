      subroutine cddcfu(le,wbg,names,units,h6,h7,h67len)
c
c -- routine to code the names + units header records that are in DCF format.
c  (high res ctd)
c author: David Newton. Scripps Instn. of Oceanog. Apr94
c
      integer mxcnum
      parameter (mxcnum=9)
      integer le,wbg(-1:mxcnum),h67len
      character names(-1:mxcnum)*8,units(-1:mxcnum)*8
      character h6*(*),h7*(*)
c
c le     ->  fortran error unit.
c wbg    -> integer array filled with start positions of .WCT info.
c            indecies in data statement below. from  wctlim.
c wen    -> same as wbg for ending column of info. from wctlim.
c qualps -> for parameters that have a quality code this array gives its
c           position in the left justified QUALT1 or QUALT2 code. If a
c           parameter doesn't have a quality byte then it is set to 0.
c             from wctlim.
c names  -> name  string for each parameter. from wctlim.
c units  -> units string for each parameter. from wctlim.
c h6     <- names line of DCF format header.
c h7     <  units line of DCF format header.
c h67lin <- length of h6 and h7 above. (number of chars)
c
c   parameters that are not recorded will have wbg(n)=0.
c
c
      integer i,kk,jj,nsig
      character scrtch*8
c
      external ljustn
c
      h6 = ' '
      h7 = ' '
c
c -- pressure.
      h6(1:6) = 'CTDPRS'
      scrtch = units(1)
      call ljustn(scrtch,nsig)
      h7(1:8) = scrtch
c
c -- temperature.
      h6(9:14) = 'CTDTMP'
      scrtch = units(2)
      call ljustn(scrtch,nsig)
      h7(9:16) = scrtch
c
c -- salinity.
      h6(17:22) = 'CTDSAL'
c     -- no units for DCF salinity.
c
c -- oxygen
      if(wbg(4) .eq. 0) then
c         -- no oxygen.
          kk = 4
      else
          h6(25:30) = 'CTDOXY'
          scrtch = units(4)
          call ljustn(scrtch,nsig)
          h7(25:32) = scrtch
          kk = 5
      endif
c
      do 40 i=5,mxcnum
         if(i .eq. 7) go to 40
         if(wbg(i) .eq. 0) then
         else
             jj = (kk-1)*8 + 1
             scrtch = names(i)
             call ljustn(scrtch,nsig)
             h6(jj:jj+7) = scrtch
             scrtch = units(i)
             call ljustn(scrtch,nsig)
             h7(jj:jj+7) = scrtch
             kk = kk + 1
         endif
 40   continue
c
c -- get here with all names and units all coded.  add the qual heading.
c
      jj = (kk-1)*8 + 1
      h6(jj:jj+3) = 'QUAL'
      h67len = jj+3
c
      return
      end
