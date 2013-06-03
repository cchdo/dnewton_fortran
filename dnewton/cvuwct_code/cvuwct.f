c program cvuwct  reads WOCE high-res ctd format and converts oxygen
c   units from ml/l to umol/kg.
c
      integer li,lo,iol,ioa,iob,icd
      integer i,fnlen,dnlen,nw,eol
      real pt,t,s,ox,sigt
      character line*80,line1*80,name*80,dirnam*50,oname*80
      intrinsic len
      external sigmp
c
      data li/5/,lo/6/,iol/12/,ioa/14/,iob/16/
      data nw/0/
c
      write(lo,*)'cvuwct converts WOCE ctd format oxy to umol/kg.'
      write(lo,*)'enter filename that contains list of files to',
     *           ' convert:'
      read(li,2010) name
 2010  format(a)
      open(iol,file=name,status='old')
c
      write(lo,*)'converted files will be put in a subdirectory:'
      write(lo,*)'enter a directory name:'
      read(li,2010) dirnam
      dnlen = len(dirnam)
      do 12 i=dnlen,1,-1
         if(dirnam(i:i).ne.' ') then
              dnlen = i
              go to 13
         endif
 12   continue
      write(lo,*)'couldn''t find end of directory name.',
     *           '  will try to continue.'
 13   continue
c
c
c -- top of filename list reading loop.
 30   continue
      read(iol,2010,iostat=icd) name
      if(icd .ne. 0) then
          if(icd .lt. 0) then
c             -- normal eof on list. end of program.
              close(iol)
              write(lo,*)nw,' stations written.'
              stop
          endif
          write(lo,*)'read err on filename list. iostat=',icd
          stop 99
      endif
      fnlen = len(name)
      do 32 i=fnlen,1,-1
         if(name(i:i).ne.' ') then
             fnlen = i
             go to 33
         endif
 32   continue
      write(lo,*)'couldn''t find end of file name.',
     *           '  will try to continue.'
 33   continue
      if(dnlen+fnlen+1 .gt. len(oname)) then
          write(lo,*)'name of file plus directory too long. max=',
     *                len(oname)-1
          write(lo,*)'dnlen,fnlen=',dnlen,fnlen
          stop 99
      endif
c
      open(ioa,file=name,status='old')
      write(lo,*)'opening: ',name
      oname = dirnam(1:dnlen) // '/' // name(1:fnlen)
      open(iob,file=oname,status='new')
c
      read(ioa,2010) line1
      read(ioa,2010) line
      do 42 i=len(line),1,-1
         if(line(i:i) .ne. ' ') then
             eol = i
             go to 43
         endif
 42   continue
      write(lo,*)'can''t find line length in data file. max=',len(line)
      write(lo,*)'line=',line
      stop 99
c
 43   continue
      write(iob,2010) line1(1:eol)
      write(iob,2010) line(1:eol)
c
c -- read and write 3rd line.
      read(ioa,2010) line
      write(iob,2010) line(1:eol)
c -- read and write 4th line.
      read(ioa,2010) line
      write(iob,2010) line(1:eol)
c -- read 5th line.
      read(ioa,2010) line
      if(index(line,'ML').ne.0 .or. index(line,'ml').ne.0) then
c         -- ML or ml found. okay.
      else
          write(lo,*)' **** ML unit not found in 5th header!.'
          write(lo,*)'   will convert anyway. ****'
      endif
      line(25:32) = ' UMOL/KG'
      write(iob,2010) line(1:eol)
c -- read and write 6th header.
      read(ioa,2010) line
      write(iob,2010) line
c
      pt = -9.0
 50   continue
      read(ioa,2010,iostat=icd) line
      if(icd .ne. 0) then
          if(icd.le.0) then
c             -- regular eof.
              nw = nw + 1
              close(ioa)
              close(iob)
              go to 30
          endif
          write(lo,*)'read err in data. iostat=',icd
          stop 99
      endif
      if(line(7:7) .ne. '.') then
           write(lo,*)'encountered blank line. assuming EOF.'
           close(ioa)
           close(iob)
           go to 30
      endif
c
      read(line,2015,iostat=icd) t,s,ox
 2015  format(8x,f8.4,f8.4,f8.3)
      if(icd .ne. 0) then
          write(lo,*)'read err on this line=',line
          stop 99
      endif
c
      if(ox .le. -3.0) then
c         -- missing oxy. nothing to do.
          write(iob,2010) line(1:eol)
          go to 50
      endif
      if(s .le. -3.0) then
c         -- missing salinity. just substitute.
          s = 34.8
      endif
      if(t .le. -3.0) then
          if(pt .gt. -3.0) then
c             -- previous temp is okay. use it.
              t = pt
          else
              write(lo,*)'subst 25.0 for missing T.'
              t = 25.
          endif
      endif
      pt = t
c
      call sigmp(0.0,0.0,t,s,sigt)
      ox = ox / (0.022392 * (sigt/1000.0 + 1.0))
c
      write(line(25:32),'(f8.1)') ox
c
      write(iob,2010) line(1:eol)
      go to 50
c
      end
          
