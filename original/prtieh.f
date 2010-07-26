      subroutine prtieh(ioc,z,p,t,tq,s,sq,ox,oxq,sil,silq,no3,no3q,
     *                  no2,no2q,po4,po4q,wild1,wild1q,wild2,wild2q,
     *                  wild3,wild3q,wldfmt,ndat,castno)
c
c -- routine to form and print an ieh format data line.
c author: David Newton. Scripps Instn. of Oceanog.
c
      integer ioc,tq(*),sq(*),oxq(*),silq(*),no3q(*),no2q(*),
     *        po4q(*),wild1q(*),wild2q(*),wild3q(*),ndat,castno
      real z(*),p(*),t(*),s(*),ox(*),sil(*),no3(*),no2(*),po4(*),
     *     wild1(*),wild2(*),wild3(*)
      character wldfmt(3)*6
c
      real dat(22),cdmiss
      integer qual(22),i,tprec,sprec
      character fn*1,recind*1,line*128
c
      intrinsic real,mod
      external rgtieh
c
      data cdmiss/3.2e4/
c
c -- initialize the arrays that go into routine rgtieh
c
      do 20 i=1,22
         dat(i) = cdmiss
         qual(i) = 9
 20   continue
      qual(1) = 0
      qual(4) = 0
c     -- cast number.
      dat(12) = real(mod(castno,100))
      qual(12) = 0
c
c -- set temp+sal precisions, footnote, and record indicator.
      tprec = 3
      sprec = 3
      fn = ' '
      recind = '3'
c
c -- loop through the data. transfering to rgtieh scheme.
      do 30 i=1,ndat
         dat(1) = z(i)
         dat(2) = t(i)
         qual(2) = tq(i)
         dat(3) = s(i)
         qual(3) = sq(i)
         dat(4) = p(i)
         dat(5) = ox(i)
         qual(5) = oxq(i)
         dat(6) = po4(i)
         qual(6) = po4q(i)
         dat(7) = sil(i)
         qual(7) = silq(i)
         dat(8) = no2(i)
         qual(8) = no2q(i)
         dat(9) = no3(i)
         qual(9) = no3q(i)
         dat(20) = wild1(i)
         qual(20) = wild1q(i)
         dat(21) = wild2(i)
         qual(21) = wild2q(i)
         dat(22) = wild3(i)
         qual(22) = wild3q(i)
c
c        -- call routine to form the data line.
         call rgtieh(dat,qual,fn,tprec,sprec,wldfmt(1),wldfmt(2),
     *               wldfmt(3),recind,line)
c        -- print it to the output.
         write(ioc,'(a)') line
 30   continue
c
      return
      end
