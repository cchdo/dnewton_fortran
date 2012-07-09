#!/usr/bin/env python

execfile('libwocecvt.py')
execfile('libwctcvt.py')
execfile('sumun1.py')
execfile('cartertab.py')

def cddcfh(line,begin,end,yr,mo,day,nobs,instr,):
  global ocntry, use_carter_tables
  global MXHDNG, _verbose

  all_items, _ = sumun1(line,begin,end)
  if _ != 0 and _verbose:
    print>>sys.stderr,'cddcfh: warning from sumun1'

  headers = ['' for i in range(6)]
  latitude = (float(all_items['LATITUDE'][0]) +
              float(all_items['LATITUDE'][1]) / 60.0) #flat
  if all_items['LATITUDE'][2] == 'W':
    latitude *= -1
  longitud = (float(all_items['LONGITUD'][0]) +
              float(all_items['LONGITUD'][1]) / 60.0) #flon
  if all_items['LONGITUD'][2] == 'S':
    longitud *= -1

  headers[1] += 'LAT %8.4f LON %9.4f NOBS %5i'%(latitude,longitud,nobs)

  if (yr  != all_items['DATE'][0] or
      mo  != all_items['DATE'][1] or
      day != all_items['DATE'][2]):
    if _verbose:
      print>>sys.stderr,"cddcfh: dates don't match; using .SUM date",
      print>>sys.stderr,'(SUM:%02d%02d%02d,'%all_items['DATE'],
      print>>sys.stderr,' WCT:%02d,%02d,%02d)'%(yr,mo,day)

  c2 = None
  if ocntry is None or ocntry.strip() == '':
    c2 = all_items['SHIP'][:2]
  else:
    c2 = ocntry

  headers[2] += 'YMD %4i %2i %2i TIME %4s SHIP %4s CC %2s' % (
      yr,mo,day,'%02d%02d'%(all_items['TIME'][0],all_items['TIME'][1]),
      all_items['SHIP'],c2)

  corrected_bottom = -9
  if all_items['BOTMCO'] != -9:
    corrected_bottom = all_items['BOTMCO']
  elif use_carter_tables and all_items['BOTMUN'] != -9:
    carea = finar(latitude, longitud)
    if carea < 0:
      if _verbose:
        print>>sys.stderr,'cddcfh: error from finar(%f,%f)'%(latitude,longitud)
    else:
      cdepth = dcorr(all_items['BOTMUN'], carea)
      if cdepth < 0:
        if _verbose:
          print>>sys.stderr,'cddcfh: error from dcorr'
      else:
        corrected_bottom = cdepth

  headers[3] += 'CRUISE %10s SECT %5s BOTM %5i' % (
        all_items['EXPOCODE'][5:15],all_items['WHPID'], corrected_bottom)
  headers[4] += 'STATION %6s     CAST %2i MINPRS %7.1f' % (
        all_items['STNNBR'],all_items['CASTNO'],-9)
  headers[5] += 'INSTRUMENT %4s   CSTDIR %1s MAXPRS %7.1f' % (
        instr, '-', -9)

  return headers
