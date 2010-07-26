      subroutine trndat(dat,qual,dqe,picky,k,bg,ctp,ctpq,ctt,cttq,
     *     cts,ctsq,ctox,ctoxq,rvp,rvpq,rvt,rvtq,bts,btsq,
     *     btox,btoxq,po4,po4q,sil,silq,no2,no2q,no3,no3q,ph,phq,
     *     wldnum,wild1,wild1q,wild2,wild2q,wild3,wild3q)
c
c -- routine to transfer the data values and codes from the "dat" and
c    "qual" arrays (from seaun1) to individual arrays.
c author: David Newton. Scripps Instn. of Oceanog.
c
c CHANGES:
c 07Aug97 dmn. added stuff for wild columns.
c
      integer mxnumo
      parameter (mxnumo=50)
      real dat(5:mxnumo)
      character qual(1:mxnumo)*1
c
      logical dqe,picky
      integer k,bg(-1:mxnumo),wldnum(3)
c
      real ctp(*),ctt(*),cts(*),ctox(*),rvp(*),rvt(*),bts(*),
     *        btox(*),po4(*),sil(*),no2(*),no3(*),ph(*),
     *        wild1(*),wild2(*),wild3(*)
      integer ctpq(*),cttq(*),ctsq(*),ctoxq(*),rvpq(*),rvtq(*),btsq(*),
     *        btoxq(*),po4q(*),silq(*),no2q(*),no3q(*),phq(*),
     *        wild1q(*),wild2q(*),wild3q(*)
c
c  dat    -> data array from seaun1.   (data from one .SEA line)
c  qual   -> quality code array from seaun1. (qual codes from one .SEA line)
c  dqe    -> .true. if want DQE mode.
c  picky  -> .true. if want picky mode.
c  k      -> the position (depth level) where you want data from "dat"
c            put in all the individual arrays.
c  bg     -> beginning column array (from sealim). used to determine if
c            a parameter was recorded in the file at all.
c lots of data arrays <-  ctp=ctdpress  ctt=ctdtemp       cts=ctdsalin
c                         ctox=ctdoxy   rvp=revers_press  rvt=revers_temp
c                         bts=botl_salt btox=botl_oxy     po4=phosphate
c                         sil=silcate   no2=nitrite       no3=nitrate
c                         ph=PH
c      these data arrays are marked with cdmiss (32000.) if missing.
c lots of quality arrays <- same names as above with 'q' appended.
c      the quality code is:  9=missing, 8=uncertain, 0=okay
c
c
      character aq*1,c*1
      logical missin
      real cdmiss,a
      data cdmiss/3.2e4/
c
      missin(c) = c.eq.'1' .or. c.eq.'5' .or. c.eq.'9'
c
c -- ctdprs
      ctp(k) = cdmiss
      ctpq(k) = 9
      a = dat(6)
      if(bg(6) .eq. 0) then
      else
          if(a .le. -8.99) then
          else
              ctp(k) = a
              ctpq(k) = 0
          endif
      endif
c
c -- ctdtmp
      ctt(k) = cdmiss
      cttq(k) = 9
      a = dat(7)
      if(bg(7) .eq. 0) then
      else
          if(a .le. -8.99) then
          else
              ctt(k) = a
              cttq(k) = 0
          endif
      endif
c
c -- ctdsal.
      cts(k) = cdmiss
      ctsq(k) = 9
      a = dat(8)
      aq = qual(8)
      if(bg(8) .eq. 0) then
      else
          if(a.le.-8.99 .or. aq.eq.'5' .or. aq.eq.'9') then
          else if(dqe) then
              cts(k) = a
              ctsq(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              cts(k) = a
              ctsq(k) = 0
              if(aq .eq. '3') ctsq(k) = 8
          endif
      endif
c
c -- ctdoxy.
      ctox(k) = cdmiss
      ctoxq(k) = 9
      a = dat(9)
      aq = qual(9)
      if(bg(9) .eq. 0) then
      else
          if(a.le.-8.99 .or. aq.eq.'5' .or. aq.eq.'9') then
          else if(dqe) then
              ctox(k) = a
              ctoxq(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              ctox(k) = a
              ctoxq(k) = 0
              if(aq .eq. '3') ctoxq(k) = 8
          endif
      endif
c
c -- revprs  (thermometric pressure)
      rvp(k) = cdmiss
      rvpq(k) = 9
      a = dat(19)
      aq = qual(19)
      if(bg(19) .eq. 0) then
      else
          if(a .le. -8.99) then
          else
              rvp(k) = a
              rvpq(k) = 0
          endif
      endif
c
c -- revtmp  (reversing thermometer temperature)
      rvt(k) = cdmiss
      rvtq(k) = 9
      a = dat(20)
      aq = qual(20)
      if(bg(20) .eq. 0) then
      else
          if(a .le. -8.99) then
          else
              rvt(k) = a
              rvtq(k) = 0
          endif
      endif
c
c -- salinity  (bottle sal).
      bts(k) = cdmiss
      btsq(k) = 9
      a = dat(11)
      aq = qual(11)
      if(bg(11) .eq. 0) then
      else
          if(a.le.-8.99 .or. missin(aq)) then
          else if(dqe) then
              bts(k) = a
              btsq(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              bts(k) = a
              btsq(k) = 0
              if(aq .eq. '3') btsq(k) = 8
          endif
      endif
c
c -- bottle oxygen.
      btox(k) = cdmiss
      btoxq(k) = 9
      a = dat(12)
      aq = qual(12)
      if(bg(12) .eq. 0) then
      else
          if(a.le.-8.99 .or. missin(aq)) then
          else if(dqe) then
              btox(k) = a
              btoxq(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              btox(k) = a
              btoxq(k) = 0
              if(aq .eq. '3') btoxq(k) = 8
          endif
      endif
c
c -- po4
      po4(k) = cdmiss
      po4q(k) = 9
      a = dat(16)
      aq = qual(16)
      if(bg(16) .eq. 0) then
      else
          if(a.le.-8.99 .or. missin(aq)) then
          else if(dqe) then
              po4(k) = a
              po4q(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              po4(k) = a
              po4q(k) = 0
              if(aq .eq. '3') po4q(k) = 8
          endif
      endif
c
c -- silicate
      sil(k) = cdmiss
      silq(k) = 9
      a = dat(13)
      aq = qual(13)
      if(bg(13) .eq. 0) then
      else
          if(a.le.-8.99 .or. missin(aq)) then
          else if(dqe) then
              sil(k) = a
              silq(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              sil(k) = a
              silq(k) = 0
              if(aq .eq. '3') silq(k) = 8
          endif
      endif
c
c -- no2 (nitrite)
      no2(k) = cdmiss
      no2q(k) = 9
      a = dat(15)
      aq = qual(15)
      if(bg(15) .eq. 0) then
      else
          if(a.le.-8.99 .or. missin(aq)) then
          else if(dqe) then
              no2(k) = a
              no2q(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              no2(k) = a
              no2q(k) = 0
              if(aq .eq. '3') no2q(k) = 8
          endif
      endif
c
c -- no3 (nitrate)
      no3(k) = cdmiss
      no3q(k) = 9
      a = dat(14)
      aq = qual(14)
      if(bg(14) .eq. 0) then
      else
          if(a.le.-8.99 .or. missin(aq)) then
          else if(dqe) then
              no3(k) = a
              no3q(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              no3(k) = a
              no3q(k) = 0
              if(aq .eq. '3') no3q(k) = 8
          endif
      endif
c
c -- ph. 
      ph(k) = cdmiss
      phq(k) = 9
      a = dat(38)
      aq = qual(38)
      if(bg(38) .eq. 0) then
      else
          if(a.le.-8.99 .or. missin(aq)) then
          else if(dqe) then
              ph(k) = a
              phq(k) = 0
          else if(aq .eq. '4') then
          else if(picky .and. aq .eq. '3') then
          else 
              ph(k) = a
              phq(k) = 0
              if(aq .eq. '3') phq(k) = 8
          endif
      endif
c
c -- wild1.
      wild1(k) = cdmiss
      wild1q(k) = 9
      if(wldnum(1) .ne. -1) then
          a = dat(wldnum(1))
          aq = qual(wldnum(1))
          if(bg(wldnum(1)) .eq. 0) then
          else
              if(a.le.-8.99 .or. missin(aq)) then
              else if(dqe) then
                  wild1(k) = a
                  wild1q(k) = 0
              else if(aq .eq. '4') then
              else if(picky .and. aq .eq. '3') then
              else 
                  wild1(k) = a
                  wild1q(k) = 0
                  if(aq .eq. '3') wild1q(k) = 8
              endif
          endif
      endif
c
c -- wild2.
      wild2(k) = cdmiss
      wild2q(k) = 9
      if(wldnum(2) .ne. -1) then
          a = dat(wldnum(2))
          aq = qual(wldnum(2))
          if(bg(wldnum(2)) .eq. 0) then
          else
              if(a.le.-8.99 .or. missin(aq)) then
              else if(dqe) then
                  wild2(k) = a
                  wild2q(k) = 0
              else if(aq .eq. '4') then
              else if(picky .and. aq .eq. '3') then
              else 
                  wild2(k) = a
                  wild2q(k) = 0
                  if(aq .eq. '3') wild2q(k) = 8
              endif
          endif
      endif
c
c -- wild3.
      wild3(k) = cdmiss
      wild3q(k) = 9
      if(wldnum(3) .ne. -1) then
          a = dat(wldnum(3))
          aq = qual(wldnum(3))
          if(bg(wldnum(3)) .eq. 0) then
          else
              if(a.le.-8.99 .or. missin(aq)) then
              else if(dqe) then
                  wild3(k) = a
                  wild3q(k) = 0
              else if(aq .eq. '4') then
              else if(picky .and. aq .eq. '3') then
              else 
                  wild3(k) = a
                  wild3q(k) = 0
                  if(aq .eq. '3') wild3q(k) = 8
              endif
          endif
      endif
c
      return
      end
