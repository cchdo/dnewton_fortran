c program wctcvt reads woce format CTD data and converts it to something
c   more reasonable. (single file DCF format)
c  
c author: David Newton. Scripps Instn. of Oceanog.
C *****************
C   PORTING NOTES:
C  completely standard FORTRAN. 
C  
C    i/o units
C          li == standard input    lo == standard output
C          le == standard error output (0 on sun, 7 on HP)
C          IOA,IOB,IOC,IOL ==  .SUM file, .WCT file, data file output, list of
C                               input .WCT files.
C   On most computers you'll just have to verify that LE is correct.
C   (initialized in a data statement below declarations)
C
C   cartertab.f uses short integers internally.
C*****************
c
c changes:
c  20Apr94 dmn. first draft.
c  11Aug95 dmn. typo in external st.
c
      integer li,lo,le,ioa,iob,ioc,iol,icd
      integer lrec,nstout,i,wchqul,mxline,
     *        mxhdng,qulmod
      parameter (mxline=300,mxhdng=19)
      integer bg(mxhdng),en(mxhdng)
      logical lsteof,samexp,docart,warn
      character line*200
      character name*40,ocntry*2
c
      integer mxcnum,mxctd
      parameter (mxcnum=9,mxctd=8000)
      integer wbg(-1:mxcnum),wen(-1:mxcnum),qualps(-1:mxcnum),
     *        cyear,cmonth,cday,nobs,nparam,h67len,nast
      real x,samphz,dat(mxctd,mxcnum)
      character names(-1:mxcnum)*8,units(-1:mxcnum)*8,cexpo*14,
     *          cwhpid*5,instru*4,qual(mxctd,2)*10
      character h1*37,h2*40,h3*39,h4*41,h5*41,h6*80,h7*80
c
      character stnnbr*6,expo*14
      integer castno
c
      intrinsic real,len,index
      external sumlim,fndsum,wctlim,wctun1,wctun2,cddcfh,cddcfu,
     *         cddcfd
c
      data li/5/,lo/6/,le/7/,ioa/10/,iob/12/,ioc/14/,iol/16/
      data nstout/0/,samexp/.true./,docart/.true./
c
      write(lo,*)' program wctcvt converts WOCE format CTD data'
      write(lo,*)'   to a more workable format.    Aug 24 1995'
      write(lo,*)' enter input .SUM filename:'
      read(li,2010) name
 2010  format(a)
c
      open(ioa,file=name,status='old')
      read(ioa,*)
      call sumlim(le,ioa,bg,en)
      if(bg(19).ne.0) then
c         -- corrected depths are available.
      else if(docart) then
          write(lo,*)' bottom depths will be corrected with',
     *               ' carter tables.'
      else
          write(lo,*)' no corrected bottom depths. no conversion.'
      endif
c
c -- ask for filename that contains the *.WCT filesnames.
      write(lo,*)' enter a file containing all the .wct filenames:'
      read(li,2010) name
      open(iol,file=name,status='old')
c
c -- ask which way to treat quality codes
      qulmod = 3
      write(lo,*)' quality code mode:'
      write(lo,*)'  1. DQE mode. All data considered good.'
      write(lo,*)'  2. Picky mode. Only good data will be allowed.'
      write(lo,*)'  3. Archive mode. Uncertain codes preserved.'
      write(lo,*)' choose a mode (1-3):'
      read(li,*) i
      
      if(i.ge.1 .and. i.le.3) then
          qulmod = i
      else
c         -- defaults to archive mode
          write(lo,*)' using Archive mode for quality codes.'
          qulmod = 3
      endif
c
c -- ask if want to look at QUALT1 or QUALT2.
      write(lo,*)' Look at QUALT1 or QUALT2? (1 or 2):'
      read(li,*) i
      wchqul = 1
      if(i .eq. 1 .or. i .eq. 2) then
          wchqul = i
      else 
          write(lo,*)' incorrect response. using QUALT1.'
      endif
c
c -- ask for originators country. 
      write(lo,*)' enter 2 char originator''s country code:'
      read(li,2010) ocntry
c
c -- ask for output filename.
      write(lo,*)' enter output filename:'
      read(li,2010) name
      open(ioc,file=name,status='new')
c
c -- loop through the list file.
      lrec = 0
      lsteof = .false.
 40   continue
      lrec = lrec + 1
      read(iol,2010,iostat=icd) name
      if(icd .ne. 0) then
          if(icd .lt. 0) then
c             -- hit eof on .lst file. go to summary.
              lsteof = .true.
              go to 400
          else
            write(le,*)'wctcvt: read err on list of files irec,iostat=',
     *                   lrec,icd
              stop 99
          endif
      endif
c
c -- open woce ctd data file.
      open(iob,file=name,status='old',iostat=icd)
c
c -- read the header info in the woce ctd file.
      call wctun1(iob,le,cexpo,cwhpid,cyear,cmonth,cday,stnnbr,
     *            castno,nobs,instru,samphz,icd)
      if(icd .ne. 0) then
          if(icd .eq. 1) then
c             -- just a warning. try to continue.
          else
              write(le,*)'wctcvt: err rtn frm wctun1. skipping file=',
     *                    name
              go to 400
          endif
      endif
c
c -- find data limits in the woce ctd data file.
      call wctlim(iob,le,wbg,wen,qualps,nast,nparam,names,units,icd) 
      if(icd .ne. 0) then
          write(le,*)'wctcvt: err rtn frm wctlim. trying to figure out',
     *               'where things are. skipping file=',name
          go to 400
      endif
c
c -- read in a whole station of data.
      warn = .false.
      call wctun2(iob,le,wbg,wen,nast,nparam,nobs,dat,qual,warn,icd)
      if(icd .ne. 0) then
          write(le,*)'wctcvt: err rtn frm wctun2. trying to read data.',
     *               'skipping file=',name
          go to 400
      endif
      if(warn) then
          write(le,*)'wctcvt: warning frm wctun2 in file=',name
      endif
c
c -- get to here to process a full station of data just read. 
 200  continue
c     -- test for out of order and repeated pressures.
      x = dat(1,1)
      do 210 i=2,nobs
         if(dat(i,1) .gt. x) then
             x = dat(i,1)
         else
c            -- data in not pressure ascending.
             write(le,*)'wctcvt: pressures non-ascending or repeated.',
     *                  'skipping file=',name,' press=',x
             go to 400
         endif
 210  continue
c
c    -- find correct .sum line.
      call fndsum(ioa,le,bg,en,stnnbr,castno,line,expo,icd)
      if(icd .ne. 0) then
          if(icd .eq. 3) then
              write(le,*)'wctcvt: cast',castno,
     *                   ' not found. will try cast 1'
              call fndsum(ioa,le,bg,en,stnnbr,1,line,expo,icd)
              if(icd .ne. 0) then
                  write(le,*)'wctcvt: cast 1 not found either.'
                  write(le,2060) stnnbr,castno
 2060             format('wctcvt: stn= ',a,' cast=',i4,' skipping.')
                  go to 400
              else
c                 -- okay, but had to use cast 1.
                  write(le,*)'wctcvt: will use .SUM info from',
     *                       ' cast 1.'
              endif
          else if(icd .eq. 1) then
              write(le,*)'wctcvt: station not found.'
              write(le,2060) stnnbr,castno
              go to 400
          endif
      endif
c
c -- check if expocode from .SUM file matches that on the .WCT file.
      i = index(expo,'/') - 1
      if(i .eq. -1) i = len(expo)
      if(expo(1:i) .ne. cexpo(1:i)) then
          samexp = .false.
      endif
c
c -- code the first 5 DCF headers.
      call cddcfh(line,le,bg,en,docart,cyear,cmonth,cday,nobs,ocntry,
     *            instru,h1,h2,h3,h4,h5)
c     -- note: minprs + maxprs still need to be coded into 4th + 5th headers.
      write(h4(35:41),'(f7.1)')dat(1,1)
      write(h5(35:41),'(f7.1)')dat(nobs,1)
c
c -- code the name + units headers.
      call cddcfu(le,wbg,names,units,h6,h7,h67len)
c
c -- output the completed header lines.
      write(ioc,2010) h1
      write(ioc,2010) h2
      write(ioc,2010) h3
      write(ioc,2010) h4
      write(ioc,2010) h5
      write(ioc,2010) h6(1:h67len)
      write(ioc,2010) h7(1:h67len)
c
c -- call routines to form and write out the data lines.
c
      warn = .false.
      call cddcfd(le,ioc,wbg,qualps,nobs,dat,qual(1,wchqul),
     *            qulmod,warn,icd)
      if(icd .ne. 0) then
          write(le,*)'wctcvt: errs above from stn/cast= ',stnnbr,'/',
     *                castno
          stop 99
      endif
      if(warn) then
         write(le,*)'wctcvt: warnings above from stn/cast= ',stnnbr,'/',
     *                castno
      endif
c
      nstout = nstout + 1
c -- close the wct data file.
      close(iob)
 400  if(lsteof) then
c         -- done with .LST file. done with everything.  normal exit.
          close(ioa)
          close(iob)
          close(ioc)
          if(.not. samexp) then
              write(le,*)' ** expocode did not match in .SUM +.WCT'
          endif
          write(le,*)' station/casts converted=',nstout
          stop
      else
          go to 40
      endif
c
      end
