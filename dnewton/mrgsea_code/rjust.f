c *******************************************************************
      subroutine rjust(string)
c -- routine to right justify a string (padding with blanks).
c
      character string*(*)
c
c  string  <->  the string to be justified.  returns right justified.
c
      integer nc,i,j,k
      intrinsic len
c
      nc = len(string)
      if(nc .le. 1) return
      if(string(nc:nc) .ne. ' ') then
c         -- string already right justified.
          return
      endif
c
c     -- look for last non-blank
      do 10 i=nc-1,1,-1
         if(string(i:i) .ne. ' ') then
             j = i
             go to 20
         endif
 10   continue
c     -- get here if line is all blank. just return
      return
c
c -- right justify 1 char at a time starting from right.
 20   k = nc-j
      do 30 i=j,1,-1
         string(i+k:i+k) = string(i:i)
         string(i:i) = ' '
 30   continue
      return
      end
