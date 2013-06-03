      subroutine ucase(string)
C -----------------------------------------------------------------------------
C  Convert all letters to upper case        russ davis routine.
C -----------------------------------------------------------------------------
C  Convert all lower case letters in character string STRING to upper case.
C  12Nov99 dmn. removed any character set dependencies.
      character string*(*)
      integer n,i,j,ia,iz,iaa
      intrinsic len,ichar,char
      ia = ichar('a')
      iz = ichar('z')
      iaa = ichar('A')
      if(.false.)write(*,*)'a=',ia,'z=',iz,'iaa=',iaa
      n=len(string)
      do 10 i=1,n
         j=ichar(string(i:i))
         if(j.ge.ia.and.j.le.iz)string(i:i)=char(j-(ia-iaa))
 10   continue
      return
      end
