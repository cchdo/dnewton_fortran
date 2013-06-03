c program wocecvt reads woce format hydrographic data and converts the
c  usual bottle or rosette cast parameters to historical units and historical
c  formats. 
c author: David Newton. Scripps Instn. of Oceanog.
C *****************
C   PORTING NOTES:
C  completely standard FORTRAN. 
C  
C    i/o units
C          li == standard input    lo == standard output
C          le == standard error output (0 on sun, 7 on HP)
C          IOA,IOB,IOC ==  .SUM file, .HYD file, data file output
C   On most computers you'll just have to verify that LE is correct.
C   (initialized in a data statement below declarations)
C
C   cartertab.f uses short integers internally.
C*****************
c
c changes:
c  19Jul93 dmn. added capability to deal with 2 bottom depths.
c  18Aug93 dmn. fixed bug in sumlim + removed debug lines from cdsd2.
c  09Sep93 dmn. fixed carter depth computation in SD2 output. (cdsd2)
c               also bug in cdieh that cause bogus text record.
c  18Nov93 dmn. modified sumlim to recognize CDEPTH and warn on improper usage.
c  21Dec93 dmn. added ndat to cdsd2 arg list.
c  28Jun96 dmn. added code to use reversing press or temp if ctd press or
c               temp is missing.
c  27Jan97 dmn. fixed bug at qualt1 qualt2 question. write(lo) not write(le)
c  07Aug97 dmn. added wild column stuff for IEH format.
c  02Feb98 dmn. added irec number to warn rtn from seaun1 message.
c
      integer li,lo,le,ioa,iob,ioc,icd
      integer irec,nstout,i,j,ndat,castsv,wchqul,mxdupe,ndupes,mxline,
     *        ninvt,mxinvt,mxhdng
      parameter (mxdupe=300,mxinvt=300,mxline=300,mxhdng=19)
      integer bg(mxhdng),en(mxhdng),wlddp(3),wldnum(3)
      real grvlat,x
      logical seaeof,frstse,samexp,iehfmt,dqe,picky,sortme,
     *        docart,dowild
      character line*200,lineh*(mxline),linesv*(mxline),stnnsv*6,
     *          c1sd2*80,c2sd2*80,c1ieh*128,c2ieh*128,wldnam(3)*7,
     *          wldfmt(3)*6,wldst1*24,wldst2*45
      character name*40,dupsta(mxdupe)*10,ivtsta(mxinvt)*10
c
      integer mxnumo
      parameter (mxnumo=50)
      integer sbg(-1:mxnumo),sen(-1:mxnumo),qualps(-1:mxnumo)
      character units(-1:mxnumo)*8,seaexp*14,qual(1:mxnumo,2)*1
      real dat(5:mxnumo)
c
      integer mxdat
      parameter (mxdat=80)
      real ctp(mxdat),ctt(mxdat),cts(mxdat),ctox(mxdat),
     *   rvp(mxdat),rvt(mxdat),bts(mxdat),btox(mxdat),po4(mxdat),
     *   sil(mxdat),no2(mxdat),no3(mxdat),ph(mxdat),p(mxdat),
     *   t(mxdat),s(mxdat),ox(mxdat),z(mxdat),wild1(mxdat),
     *   wild2(mxdat),wild3(mxdat)
      integer ctpq(mxdat),cttq(mxdat),ctsq(mxdat),ctoxq(mxdat),
     *   rvpq(mxdat),rvtq(mxdat),btsq(mxdat),btoxq(mxdat),po4q(mxdat),     
     *   silq(mxdat),no2q(mxdat),no3q(mxdat),phq(mxdat),pq(mxdat),
     *   tq(mxdat),sq(mxdat),oxq(mxdat),wild1q(mxdat),wild2q(mxdat),
     *   wild3q(mxdat)
      character stnnbr*6,comm*30,expo*14
      integer castno,k,nast
      logical warn
c
      intrinsic real,len,index
      external sumlim,sealim,fndsum,seaqwk,seaun1,cdieh,trndat,dodept,
     *         dounit,prtieh,chkdep,cdsd2,prtsd2,namget,mkwlds
c
      data li/5/,lo/6/,le/6/,ioa/10/,iob/12/,ioc/14/
      data nstout/0/,ndupes/0/,samexp/.true./,docart/.true./
      data wldnum/-2,-2,-2/
c
      write(lo,*)' program wocecvt converts WOCE format hydro data'
      write(lo,*)'   to more familar formats.    Feb 5 1998'
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
c -- ask for .SEA filename.
      write(lo,*)' enter input .SEA (.HYD) filename:'
      read(li,2010) name
      open(iob,file=name,status='old')
      call sealim(iob,le,sbg,sen,qualps,nast,units,seaexp,icd)
      if(icd .ne. 0) then
          write(le,*)'main: err rtn frm sealim. icd=',icd
          stop 99
      endif
c
c -- ask which way to treat quality codes
      dqe = .false.
      picky = .false.
      write(lo,*)' quality code mode:'
      write(lo,*)'  1. DQE mode. All data considered good.'
      write(lo,*)'  2. Picky mode. Only good data will be allowed.'
      write(lo,*)'  3. Archive mode. Uncertain codes preserved.'
      write(lo,*)' choose a mode (1-3):'
      read(li,*) i
      
      if(i .eq. 1) then
          dqe = .true.
      else if(i .eq. 2) then
          picky = .true.
      else
c         -- defaults to archive mode
          write(lo,*)' using Archive mode for quality codes.'
      endif
c
c -- ask if want to look at QUALT1 or QUALT2.
      write(lo,*)' Look at QUALT1 or QUALT2? (1 or 2):'
      read(li,*) i
      wchqul = 1
      if(i .eq. 2) then
          if(sbg(0) .eq. 0) then
              write(lo,*)' no QUALT2 in data. will use 1'
          else
              wchqul = 2
          endif
      endif
c
c -- ask for output format type.
      write(lo,*)' what kind of output? 1==IEH  2==SD2'
      read(li,*) i
      if(i .eq. 2) then
          iehfmt = .false.
          write(lo,*)' will output in SD2 format.'
      else
          iehfmt = .true.
          write(lo,*)' will output in IEH format.'
      endif
c
c -- try to find 'wildcol' file if IEH format and want wild columns filled.
      dowild = .false.
      if(iehfmt) then
          open(ioc,file='wildcol',status='old',iostat=icd)
          if(icd .ne. 0) then
              write(lo,*)'no ''wildcol'' file found.'
              close(ioc)
          else
c             -- read the 3 wild column lines
              do 30 i=1,3
                 read(ioc,'(a7,1x,i1)',iostat=icd) wldnam(i),wlddp(i)
                 if(icd .ne. 0) then
                     write(le,*)'read error in ''wildcol'' at line ',i
                     stop 99
                 endif
                 if(wlddp(i) .gt. 6) then
                     write(le,*)'invalid wild-decimal in ',
     *                          '''wildcol'' max=6'
                     stop 99
                 endif
                 call namget(wldnam(i),wldnam,j)
                 if(j .eq. -3) then
                     write(le,*)'unrecognized name in ''wildcol''',
     *                          ' file. name=',wldnam(i)
                     write(le,*)' will continue as if NONE on line=',i
                     j = -2
                 endif
                 wldnum(i) = j
                 if(wldnum(i) .ne. -2) then
                     if(sbg(wldnum(i)) .eq. 0) then
                         write(le,*)'SEA file does not contain:',
     *                              wldnam(i),'. will continue.'
                     endif
                 endif
 30           continue
              close(ioc)
              dowild = .true.
          endif
      endif
c
c -- ask for output filename.
      write(lo,*)' enter output filename:'
      read(li,2010) name
      open(ioc,file=name,status='new')
c
c -- loop through the .SEA file.
      seaeof = .false.
      frstse = .true.
      irec = 4
 40   continue
      irec = irec + 1
      read(iob,2010,iostat=icd) lineh
      if(icd .ne. 0) then
          if(icd .lt. 0) then
c             -- hit eof on .SEA file. go to processing.
              seaeof = .true.
              go to 200
          else
              write(le,*)'main: read err on .SEA irec,iostat=',
     *                   irec,icd
              stop 99
          endif
      endif
c
 41   continue
      call seaqwk(lineh,le,sbg,sen,stnnsv,castsv,icd)
      if(icd .ne. 0) then
          write(le,*)'main: skipping .SEA line. irec=',irec
          go to 40
      endif
      if(frstse) then
c         -- first .sea line for a station/cast
          ndat = 0
          stnnbr = stnnsv
          castno = castsv
          frstse = .false.
      else
c         -- get here with 2nd and subsequent lines in a cast.
          if(castno .ne. castsv .or. stnnbr .ne. stnnsv) then
c             -- have encountered a new cast or station. save the line.
c                process the station/cast just finished reading.
              linesv = lineh
              go to 200
          endif
      endif
c
c -- decode the .SEA line.
      ndat = ndat + 1
      call seaun1(lineh,le,sbg,sen,qualps,nast,stnnbr,castno,
     *            dat,qual,warn)
      if(warn) then 
          write(le,*)'main: warn rtn frm seaun1. irec=',irec
      endif
c
c -- save the data we want into arrays now.
      k = ndat
      call trndat(dat,qual(1,wchqul),dqe,picky,k,sbg,ctp,ctpq,
     *            ctt,cttq,cts,ctsq,ctox,ctoxq,
     *            rvp,rvpq,rvt,rvtq,bts,btsq,btox,btoxq,
     *            po4,po4q,sil,silq,no2,no2q,
     *            no3,no3q,ph,phq,wldnum,wild1,wild1q,
     *            wild2,wild2q,wild3,wild3q)
c
c     -- reconcile mulitple sources of some data. p,t,s,ox
c  note: set up for normal hydrographic params from rosette casts. other
c        cast types (LVS ,BOT) will require different choices.
c     -- use ctd pressure .
      p(k) = ctp(k)
      pq(k) = ctpq(k)
      if(ctpq(k).eq.9 .and. rvpq(k).ne.9) then
c         -- no ctdprs, use revprs if available.
          p(k) = rvp(k)
          pq(k) = rvpq(k)
      endif
c     -- use ctd temperature. 
      t(k) = ctt(k)
      tq(k) = cttq(k)
      if(cttq(k).eq.9 .and. rvtq(k).ne.9) then
c         -- no ctdtmp, use revtmp if available.
          t(k) = rvt(k)
          tq(k) = rvtq(k)
      endif
c     -- use BOTTLE salinity unless ctdsal is all that's available.
c        (no swap if dqe is true)
      s(k) = bts(k)
      sq(k) = btsq(k)
      if(.not. dqe .and. sq(k) .eq. 9) then
          s(k) = cts(k)
          sq(k) = ctsq(k)
      endif
c     -- use bottle oxygens.
      ox(k) = btox(k)
      oxq(k) = btoxq(k)
c
c     -- test for missing pressure. if it is missing. junk all the data
c        from this level.
      if(pq(k) .eq. 9) then
c         -- decrement ndat so this garbage gets written over next time.
          ndat = ndat - 1
      endif
c
c     -- go up and read the next .SEA line.
      go to 40
c
c -- get to here to process a full station of data just read. 
 200  continue
c     -- test for out of order pressures.
      sortme = .false.
      x = p(1)
      do 210 i=1,ndat
         if(p(i) .ge. x) then
             x = p(i)
         else
c            -- non - ascending pressures.
             sortme = .true.
c            -- test for really inverted.
             if(x-p(i) .ge. 3.) then
                 write(le,*)'main: big press inversion. stn=',stnnbr,
     *                      'cst=',castno
                 go to 211
             endif
         endif
 210  continue
 211  if(sortme) then
c         -- store away stations that had ascending depths.
          ninvt = ninvt + 1
          if(ninvt .gt. mxinvt) then
              write(le,*)'main: prgrmr err. increase mxinvt'
              write(le,*)'      too many stations with ascend. depths.'
              ninvt = ninvt -1
          endif
          write(ivtsta(ninvt),'(a,''/'',i3)') stnnbr,castno
c         -- call sort on pressure.
c         -- call routine to reorder everything.
      endif
          
c     -- find correct .sum line.
      call fndsum(ioa,le,bg,en,stnnbr,castno,line,expo,icd)
      if(icd .ne. 0) then
          if(icd .eq. 3) then
              write(le,*)'main: cast',castno,
     *                   ' not found. will try cast 1'
              call fndsum(ioa,le,bg,en,stnnbr,1,line,expo,icd)
              if(icd .ne. 0) then
                  write(le,*)'main: cast 1 not found either.'
                  write(le,2060) stnnbr,castno
 2060             format('main: stn= ',a,' cast=',i4,' skipping.')
                  go to 400
              else
c                 -- okay, but had to use cast 1.
                  write(le,*)'main: will use .SUM info from',
     *                       ' cast 1.'
              endif
          else if(icd .eq. 1) then
              write(le,*)'main: station not found.'
              write(le,2060) stnnbr,castno
              go to 400
          endif
      endif
c
c -- check if expocode from .SUM file matches that on the .SEA file.
      i = index(expo,'/') - 1
      if(i .eq. -1) i = len(expo)
      if(expo(1:i) .ne. seaexp(1:i)) then
          samexp = .false.
      endif
c
c     -- write out station header.
c
      if(iehfmt) then
          call cdieh(line,le,bg,en,docart,grvlat,comm,
     *               c1ieh,c2ieh,icd)
          if(dowild) then
              wldst1 = ' '
              wldst2 = ' '
              call mkwlds(wldnam,wlddp,wldnum,units,wldfmt,
     *                    wldst1,wldst2)
              c1ieh(104:127) = wldst1
              c2ieh(83:127) = wldst2
          endif
          write(ioc,2010) c1ieh
          write(ioc,2010) c2ieh
          if(comm(1:1).ne.' ')
     *        write(ioc,'(a30,33x,''Z*'',t128,''9'')')comm
      else
          call cdsd2(line,le,bg,en,docart,ndat,grvlat,
     *               c1sd2,c2sd2,icd)
          write(ioc,2010) c1sd2
          write(ioc,2010) c2sd2
      endif
c    
c -- calculate depth from pressure.
      call dodept(p,t,tq,s,sq,ndat,grvlat,z)
c
c -- check for depth sequencing and duplicate depths.
      call chkdep(z,ndat,icd)
      if(icd .eq. 2) then
          write(le,*)'main: depth sequencing prob. stn=',stnnbr,
     *               'cst=',castno
      else if(icd .eq. 1) then
          ndupes = ndupes + 1
          if(ndupes .gt. mxdupe) then
              write(le,*)'main: prgrmr err. increase mxdupe'
              write(le,*)'      too many stations with dup. depths.'
              ndupes = ndupes -1
          endif
          write(dupsta(ndupes),'(a,''/'',i3)') stnnbr,castno
      endif
c
c -- do unit conversion.
      call dounit(sbg,units,p,t,tq,s,sq,ox,oxq,sil,silq,
     *            no3,no3q,no2,no2q,po4,po4q,ndat)
c 
c -- call routines to form and write out the data lines.
c
      if(iehfmt) then
          call prtieh(ioc,z,p,t,tq,s,sq,ox,oxq,sil,silq,no3,no3q,
     *                no2,no2q,po4,po4q,wild1,wild1q,wild2,wild2q,
     *                wild3,wild3q,wldfmt,ndat,castno)
      else
c         -- sd2 format.
          call prtsd2(ioc,z,t,tq,s,sq,ox,oxq,sil,silq,no3,no3q,
     *                no2,no2q,po4,po4q,ph,phq,ndat,castno)
      endif
c
c
      nstout = nstout + 1
 400  if(seaeof) then
c         -- done with .SEA file. done with everything.  normal exit.
          close(ioa)
          close(iob)
          close(ioc)
          if(.not. samexp) then
              write(le,*)' ** expocode did not match in .SUM +.SEA'
          endif
          if(ninvt .gt. 0) then
              write(le,*)' following stn/csts had inverted pressures:'
              write(le,'(7(1x,a))') (ivtsta(i),i=1,ninvt)
          endif
          if(ndupes .gt. 0) then
              write(le,*)' following stn/csts had duplicate depths:'
              write(le,'(7(1x,a))') (dupsta(i),i=1,ndupes)
          endif
          write(le,*)' station/casts converted=',nstout
          stop
      else
          line = linesv
          frstse = .true.
          go to 41
      endif
c
      end
