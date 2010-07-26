      subroutine prtsd2(ioc,z,t,tq,s,sq,ox,oxq,sil,silq,no3,no3q,
     *                  no2,no2q,po4,po4q,ph,phq,ndat,castno)
c
c -- routine to form and print an ieh format data line.
c author: David Newton. Scripps Instn. of Oceanog.
c
      integer ioc,tq(*),sq(*),oxq(*),silq(*),no3q(*),no2q(*),
     *        po4q(*),phq(*),ndat,castno
      real z(*),t(*),s(*),ox(*),sil(*),no3(*),no2(*),po4(*),
     *        ph(*)
c
      real cdmiss,dat(10)
      integer i,prec(10)
      character alph*31,line*80
c
      external wrtsd2
c
      data cdmiss/3.2e4/
c
      do 10 i=1,10
         dat(i) = cdmiss
         prec(i) = 9
 10   continue
c
c -- alph is a character string that passes character info to wrtsd2.
      alph = ' '
c     -- initialize missing sigma_stp, missing sound_speed, castno,record_type
      alph(9:9) = '9'
      alph(15:15) = '9'
      write(alph(26:26),'(i1)') mod(castno,10)
      alph(30:31) = '33'
c
c -- loop through the data. transfering to wrtsd2 scheme.
      do 30 i=1,ndat
c
c        -- depth (1)
         dat(1) = z(i)
         prec(1) = 0
c
c        -- temp (2)
         dat(2) = t(i)
         prec(2) = 9
         alph(3:3) = ' '
         if(tq(i) .ne. 9) then
             prec(2) = 3
             if(tq(i) .eq. 8) alph(3:3) = '8'
         endif
c
c        -- sal (3)
         dat(3) = s(i)
         prec(3) = 9
         alph(4:4) = ' '
         if(sq(i) .ne. 9) then
             prec(3) = 3
             if(sq(i) .eq. 8) alph(4:4) = '8'
         endif
c
c        -- oxygen (4)
         dat(4) = ox(i)
         prec(4) = 9
         alph(16:16) = ' '
         if(oxq(i) .ne. 9) then
             prec(4) = 2
             if(oxq(i) .eq. 8) alph(16:16) = '8'
         endif
c
c        -- po4 (5)
         dat(5) = po4(i)
         prec(5) = 9
         if(po4q(i) .ne. 9) then
             prec(5) = 2
             if(dat(5) .lt. .005) prec(5) = 4
             if(dat(5) .gt. 99.99) prec(5) = 5
         endif
c
c        -- woce doesn't measure total phosphorus. (6)
c
c        -- silicate (7)
         dat(7) = sil(i)
         prec(7) = 9
         if(silq(i) .ne. 9) then
             prec(7) = 1
             if(dat(7) .lt. .05) prec(7) = 4
             if(dat(7) .gt. 999.9) prec(7) = 5
         endif
c
c        -- no2 (nitrite) (8)
         dat(8) = no2(i)
         prec(8) = 9
         if(no2q(i) .ne. 9) then
             prec(8) = 2
             if(dat(8) .lt. .005) prec(8) = 4
             if(dat(8) .gt. 9.99) prec(8) = 5
         endif
c
c        -- no3 (nitrate) (9)
         dat(9) = no3(i)
         prec(9) = 9
         if(no3q(i) .ne. 9) then
             prec(9) = 1
             if(dat(9) .lt. .05) prec(9) = 4
             if(dat(9) .gt. 99.9) prec(9) = 5
         endif
c
c        -- ph (10)
         dat(10) = ph(i)
         prec(10) = 9
         if(phq(i) .ne. 9) then
             prec(10) = 2
             if(dat(10) .lt. .005) prec(10) = 4
             if(dat(10) .gt. 9.99) prec(10) = 5
         endif
c
c        -- call routine to form the data line.
         call wrtsd2(dat,prec,alph,line)
c        -- last level gets different next record indicator
         if(i.eq.ndat) line(79:79) = '1'
c        -- print it to the output.
         write(ioc,'(a)') line
 30   continue
c
      return
      end
