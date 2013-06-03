c program sumchk. checks woce .SUM files every way it can. 
c author: David Newton. Scripps Instn. of Oceanog.
      integer li,lo,le,ioa,icd
      integer mxhdng
      parameter (mxhdng=19)
      integer bg(mxhdng),en(mxhdng),i,nwarn,nsum,mxwarn
      real x
      character line*200
      character name*40
c
      character ship*4,expo*14,whpid*5,stnnbr*6,ctype*3,tcode*2,
     *          lathem*1,lonhem*1,nav*3,params*20,comm*30
      integer castno,year,month,day,time,latdeg,londeg,botmun,
     *        botmco,hab,wheel,mxpres,nbots
      real latmin,lonmin
      logical warn,smfrst
c
      intrinsic nint
c
      data li/5/,lo/6/,le/7/,ioa/10/,nwarn/0/,nsum/0/,mxwarn/20/
c
      write(lo,*)' program sumchk verifies woce .SUM files.'
      write(lo,*)' enter input .SUM filename:'
      read(li,'(a)') name
c
      write(lo,*)' enter maximum number of errors:'
      read(li,*) x
      mxwarn = nint(x)
c
      open(ioa,file=name,status='old')
      read(ioa,*)
      call sumlim(le,ioa,bg,en)
      write(lo,*)
      read(ioa,*)
c
      smfrst = .true.
      do 10 i=5,1000
         warn = .false.
         read(ioa,'(a)',end=20) line
         call sumqwk(line,le,bg,en,smfrst,expo,stnnbr,castno,
     *                ctype,tcode,icd)
         if(icd .ne. 0) then
             if(icd .eq. -1) then
                 go to 10
             else
                 write(le,*)'main: decode err in sumqwk. rec=',i
             endif
         endif
         nsum = nsum + 1
         call sumun1(line,le,bg,en,ship,expo,whpid,stnnbr,
     *                castno,ctype,year,month,day,time,tcode,
     *                latdeg,latmin,lathem,londeg,lonmin,lonhem,
     *                nav,botmun,botmco,hab,wheel,mxpres,nbots,
     *                params,comm,warn)
         if(warn) then
             write(le,*)'main: warning on rec=',i
             nwarn = nwarn + 1
             if(nwarn .gt. mxwarn) then
                  write(lo,*)'main: too many warnings. aborting. **'
                  go to 20
             endif
         endif
 10   continue
      write(le,*)'main: too many lines in .SUM file.'
      stop 99
c
 20   continue
      write(lo,*)' data lines examined=',nsum
      write(lo,*)' check completed. number of lines w/warnings=',
     *            nwarn
      close(ioa)
      stop
      end
