      subroutine mkwlds(wldnam,wlddp,wldnum,units,wldst1,wldst2)
c
c     -- routine to make wild column header strings for ieh format lines.
c
      character wldnam(3)*7,wldst1*24,wldst2*45
      character units(-1:*)*8
      integer wlddp(3),wldnum(3)
c
      if(wldnum(1) .ne. -2) then
          wldst1(2:8) = wldnam(1)
          wldst2(1:1) = 'I'
          write(wldst2(4:9),2010) wlddp(1)
 2010      format('(F7.',i1,')')
          wldst2(22:29) = units(wldnum(1))
      endif
c
      if(wldnum(2) .ne. -2) then
          wldst1(10:16) = wldnam(2)
          wldst2(2:2) = 'I'
          write(wldst2(10:15),2010) wlddp(2)
          wldst2(30:37) = units(wldnum(2))
      endif
c
      if(wldnum(3) .ne. -2) then
          wldst1(18:24) = wldnam(3)
          wldst2(3:3) = 'I'
          write(wldst2(16:21),2010) wlddp(3)
          wldst2(38:45) = units(wldnum(3))
      endif
c
      return
      end
