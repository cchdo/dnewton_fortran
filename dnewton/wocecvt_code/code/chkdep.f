      subroutine chkdep(z,ndat,icd)
c
c -- short routine to check for miss sequenced or duplicate depths.
c author: David Newton. Scripps Instn. of Oceanog.
c
      real z(*)
      integer ndat,icd
c
c  z    -> array of depths 
c  ndat -> number of depths in z
c  icd  <- return code. 0==okay  1==duplicate depths  2==depths miss-sequenced.
c
      integer zi,zj,i
      intrinsic nint
c
      icd = 0
      zi = nint(z(1))
      do 10 i=2,ndat
         zj = nint(z(i))
         if(zj .lt. zi) then
c            -- depths not deeper going down.
             icd = 2
             return
         else if(zj .eq. zi) then
c            -- duplicate depth.
             icd = 1
         endif
         zi = zj
 10   continue
      return
      end
