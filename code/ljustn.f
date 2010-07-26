c *******************************************************************
      subroutine ljustn(string,nsig)
c -- routine to left justify a string (padding with blanks) and return
c    number of significant characters.
c
      character string*(*)
      integer nsig
c
c  string  <->  the string to be justified.  returns left justified.
c  nsig    <-   the number of significant characters.
c
      integer nc,i,i1,ii
      intrinsic len
c
      nsig = 0
      nc = len(string)
c     -- look for first non-blank
      do 10 i=1,nc
         if(string(i:i) .ne. ' ') then
             i1 = i
             go to 20
         endif
 10   continue
      return
c
 20   continue
      if(i1 .gt. 1) then
          ii = 0
          do 25 i=i1,nc
             ii = ii + 1
             string(ii:ii) = string(i:i)
             string(i:i) = ' '
 25       continue
      endif
c     -- get to here with string left justified. count from right now.
      do 30 i=nc,1,-1
         if(string(i:i) .ne. ' ') then
             nsig = i
             return
         endif
 30   continue
      write(*,*)' in LJUSTN: dropped out of 30 loop. something''s',
     *          ' wrong.'
      stop 99
      end
