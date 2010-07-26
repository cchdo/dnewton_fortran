      subroutine wrtsd2(dat,prec,alph,line)
c
c -- routine to reformat an sd2 data line that was decoded using
c    subroutine rdsd2.
c author: David Newton. Scripps Instn. of Oceanog.
c
      real dat(*)
      integer prec(*)
      character alph*31,line*80
c
      real a
c
      intrinsic nint
c
 3001 format(i1.1)
 3002 format(i2.2)
 3003 format(i3.3)
 3004 format(i4.4)
 3005 format(i5.5)
c
      line = ' '
c
c -- depth.
      a = dat(1)
      write(line(1:5),3005) nint(a)
c     -- depth qual and thermometric flag.
      line(6:7) = alph(1:2)
c
c -- temp.
      a = dat(2)
      if(prec(2) .eq. 9) then
          line(13:13) = '9'
      else
          if(prec(2) .eq. 3) then
              write(line(8:12),'(i5.4)') nint(a*1000.)
              line(13:13) = '3'
          else if(prec(2) .eq. 2) then
              write(line(8:11),'(i4.3)') nint(a*100.)
              line(13:13) = '2'
          else if(prec(2) .eq. 1) then
              write(line(8:10),'(i3.2)') nint(a*10.)
              line(13:13) = '1'
          else
              write(*,*)' invalid temp. prec.=',prec(2)
              stop 99
          endif
          line(14:14) = alph(3:3)
      endif
c
c -- salinity.
      a = dat(3)
      if(prec(3) .eq. 9) then
          line(20:20) = '9'
      else
          if(prec(3) .eq. 3) then
              write(line(15:19),3005) nint(a*1000.)
              line(20:20) = '3'
          else if(prec(3) .eq. 2) then
              write(line(15:18),3004) nint(a*100.)
              line(20:20) = '2'
          else if(prec(3) .eq. 1) then
              write(line(15:17),3003) nint(a*10.)
              line(20:20) = '1'
          else
              write(*,*)' invalid sal. prec.=',prec(3)
              stop 99
          endif
          line(21:21) = alph(4:4)
      endif
c
c -- copy sigma-t and sound speed straight to output line.
      line(22:26) = alph(5:9)
      line(27:32) = alph(10:15)
c
c -- oxygen.
      a = dat(4)
      if(prec(4) .eq. 9) then
          line(37:37) = '9'
      else
          if(prec(4) .eq. 2) then
              write(line(33:36),3004) nint(a*100.)
              line(37:37) = '2'
          else if(prec(4) .eq. 1) then
              write(line(33:35),3003) nint(a*10.)
              line(37:37) = '1'
          else
              write(*,*)' invalid oxy. prec.=',prec(4)
              stop 99
          endif
          line(38:38) = alph(16:16)
      endif
c
c -- copy data range check flags and cast time if they even appear.
c    cast number too.
      line(39:44) = alph(17:22)
      line(45:47) = alph(23:25)
      line(48:48) = alph(26:26)
c
c -- phosphate.
      a = dat(5)
      if(prec(5) .eq. 9) then 
          line(53:53) = '9'
      else
          if(prec(5) .eq. 2) then
              write(line(49:52),3004) nint(a*100.)
              line(53:53) = '2'
          else if(prec(5) .eq. 1) then
              write(line(49:51),3003) nint(a*10.)
              line(53:53) = '1'
          else if(prec(5) .eq. 0) then
              write(line(49:50),3002) nint(a)
              line(53:53) = '0'
          else if(prec(5) .eq. 4) then
              line(49:52) = '0000'
              line(53:53) = '4'
          else if(prec(5) .eq. 5) then
              line(49:52) = '9999'
              line(53:53) = '5'
          else
              write(*,*)' invalid po4 prec.=',prec(5)
              stop 99
          endif
      endif
c
c -- total phosphorus
      a = dat(6)
      if(prec(6) .eq. 9) then 
          line(58:58) = '9'
      else
          if(prec(6) .eq. 2) then
              write(line(54:57),3004) nint(a*100.)
              line(58:58) = '2'
          else if(prec(6) .eq. 1) then
              write(line(54:56),3003) nint(a*10.)
              line(58:58) = '1'
          else if(prec(6) .eq. 0) then
              write(line(54:55),3002) nint(a)
              line(58:58) = '0'
          else if(prec(6) .eq. 4) then
              line(54:57) = '0000'
              line(58:58) = '4'
          else if(prec(6) .eq. 5) then
              line(54:57) = '9999'
              line(58:58) = '5'
          else
              write(*,*)' invalid tot. phos. prec=',prec(6)
              stop 99
          endif
      endif
c
c -- silicate.
      a = dat(7)
      if(prec(7) .eq. 9) then 
          line(63:63) = '9'
      else
          if(prec(7) .eq. 1) then
              write(line(59:62),3004) nint(a*10.)
              line(63:63) = '1'
          else if(prec(7) .eq. 0) then
              write(line(59:61),3003) nint(a)
              line(63:63) = '0'
          else if(prec(7) .eq. 4) then
              line(59:62) = '0000'
              line(63:63) = '4'
          else if(prec(7) .eq. 5) then
              line(59:62) = '9999'
              line(63:63) = '5'
          else
              write(*,*)' invalid sio3 prec.=',prec(7)
              stop 99
          endif
      endif
c
c -- nitrite (no2)
      a = dat(8)
      if(prec(8) .eq. 9) then 
          line(67:67) = '9'
      else
          if(prec(8) .eq. 2) then
              write(line(64:66),3003) nint(a*100.)
              line(67:67) = '2'
          else if(prec(8) .eq. 1) then
              write(line(64:65),3002) nint(a*10.)
              line(67:67) = '1'
          else if(prec(8) .eq. 0) then
              write(line(64:64),3001) nint(a)
              line(67:67) = '0'
          else if(prec(8) .eq. 4) then
              line(64:66) = '000'
              line(67:67) = '4'
          else if(prec(8) .eq. 5) then
              line(64:66) = '999'
              line(67:67) = '5'
          else
              write(*,*)' invalid no2 prec=',prec(8)
              stop 99
          endif
      endif
c
c -- nitrate (no3)
      a = dat(9)
      if(prec(9) .eq. 9) then 
          line(71:71) = '9'
      else
          if(prec(9) .eq. 1) then
              write(line(68:70),3003) nint(a*10.)
              line(71:71) = '1'
          else if(prec(9) .eq. 0) then
              write(line(68:69),3002) nint(a)
              line(71:71) = '0'
          else if(prec(9) .eq. 4) then
              line(68:70) = '000'
              line(71:71) = '4'
          else if(prec(9) .eq. 5) then
              line(68:70) = '999'
              line(71:71) = '5'
          else
              write(*,*)' invalid no3 prec=',prec(9)
              stop 99
          endif
      endif
c
c -- ph.
      a = dat(10)
      if(prec(10) .eq. 9) then 
          line(75:75) = '9'
      else
          if(prec(10) .eq. 2) then
              write(line(72:74),3003) nint(a*100.)
              line(75:75) = '2'
          else if(prec(10) .eq. 1) then
              write(line(72:73),3002) nint(a*10.)
              line(75:75) = '1'
          else if(prec(10) .eq. 0) then
              write(line(72:72),3001) nint(a)
              line(75:75) = '0'
          else if(prec(10) .eq. 4) then
              line(72:74) = '000'
              line(75:75) = '4'
          else if(prec(10) .eq. 5) then
              line(72:74) = '999'
              line(75:75) = '5'
          else
              write(*,*)' invalid ph prec=',prec(10)
              stop 99
          endif
      endif
c
c -- copy density inversion flag and record types
c
      line(76:80) = alph(27:31)
c
      return
      end
