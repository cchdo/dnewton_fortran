#!/usr/bin/env python

execfile('libwocecvt.py')
execfile('libwctcvt.py')

REQUIRED = ['EXPOCODE', 'WHP', 'DATE', 'STNNBR', 'CASTNO',
            'NO. RECORDS', 'INSTRUMENT NO.', 'SAMPLING RATE',]

def WCTUN1_ERR(d, errlv):
  ''' WCTUN1_ERR(2)
      Fill in remaining parameters with None when an error is encountered
      in wctun1().
      d        dictionary of all parameters
      errlv    error level to return
  '''
  for key in REQUIRED:
    if key not in d:
      d[key] = None
  return (d, errlv)

def wctun1(lst):
  global _verbose,
  warned = False
  all_items = {}

  if len(lst) == 0:
    return WCTUN1_ERR(all_items, 2)

  line = lst[0]

  if line.find('EXPOCODE') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no EXPOCODE found'
    return WCTUN1_ERR(all_items, 2)

  if line.find('WHP') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no WHPID found'
    return WCTUN1_ERR(all_items, 2)

  if line.find('EXPOCODE')+8 > line.find('WHP')-1:
    if _verbose:
      print>>sys.stderr,'wctun1: strange first header line (whpid, expo)'
    return WCTUN1_ERR(all_items, 2)
  scrtch = line[line.find('EXPOCODE')+8:line.find('WHP')+1].strip()
  all_items['EXPOCODE'] = scrtch[:scrtch.find(' ')]

  if line.find('DATE') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no DATE word found on line 1'
    return WCTUN1_ERR(all_items, 2)

  if line.find('WHP')+6 > line.find('DATE')-1:
    if _verbose:
      print>>sys.stderr,'wctun1: strange first header line (date, whpid)'
    return WCTUN1_ERR(all_items, 2)
  all_items['WHP'] = line[line.find('WHP')+6:line.find('DATE')-1].strip()

  scrtch = line[line.find('DATE')+4:line.find('DATE')+14].strip()
  cmonth, cday, cyear = -9, -9, -9

  try:
    delims = (2,4)
    if len(scrtch) == 5:
      if _verbose:
        print>>sys.stderr,'5-char date -- please fix; read i1,2i2'
      delims = (1,3)
    cmonth, cday, cyear = (int(scrtch[:delims[0]]),
                           int(scrtch[delims[0]:delims[1]]),
                           int(scrtch[delims[1]:]))
  except (TypeError, ValueError):
    if _verbose:
      print>>sys.stderr,'invalid date %s' % scrtch
    cmonth, cday, cyear = -9, -9, -9
    warned = True
  all_items['DATE'] = {'month':cmonth,'day':cday,'year':cyear} # FIXME

  line = list[1]
  if line.find('STNNBR') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no STNNBR found on line 2'
    return WCTUN1_ERR(all_items, 2)

  if line.find('CASTNO') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no CASTNO found on line 2'
    return WCTUN1_ERR(all_items, 2)

  if line.find('STNNBR')+6 > line.find('CASTNO')-1:
    if _verbose:
      print>>sys.stderr,'wctun1: strange 2nd header line (stnnbr, castno)'
    return WCTUN1_ERR(all_items, 2)
  all_items['STNNBR'] = line[line.find('STNNBR')+6:line.find('CASTNO')].strip()

  if line.find('NO.') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no NO. RECORDS= found on line 2'
    return WCTUN1_ERR(all_items, 2)

  if (line.find('NO.')-1) - (line.find('CASTNO')+6) + 1 < 1:
    if _verbose:
      print>>sys.stderr,'wctun1: strange 2nd header line (no.records, castno)'
    return WCTUN1_ERR(all_items, 2)

  try:
    all_items['CASTNO'] = int(line[line.find('NO.')-1:line.find('CASTNO')+6])
  except (TypeError, ValueError):
    if _verbose:
      print>>sys.stderr,'wctun1: invalid castno %s' % line[
          line.find('NO.')-1:line.find('CASTNO')+6]
    return WCTUN1_ERR(all_items, 2)

  if line.find('ORDS=') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no NO. RECORDS= found on line 2'
    return WCTUN1_ERR(all_items, 2)

  scrtch = line[line.find('ORDS=')+5:line.find('ORDS=')+5+10].strip()
  if scrtch.find(' ') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: NO. RECORDS= (line 2) unreadable'
    return WCTUN1_ERR(all_items, 2)

  try:
    all_items['NO. RECORDS'] = int(scrtch[:scrtch.find(' ')].strip())
  except (TypeError, ValueError):
    if _verbose:
      print>>sys.stderr,'wctun1: invalid NO. RECORDS= %s' % scrtch[
          :scrtch.find(' ')].strip()
    return WCTUN1_ERR(all_items, 2)

  line = lst[2]

  hdr3_bad = False
  if line.find('NO.') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no INSTRUMENT NO. found on line 3'
    hdr3_bad = True
  if line.find('SAMP') == -1:
    if _verbose:
      print>>sys.stderr,'wctun1: no SAMPLING RATE found on line 3'
    hdr3_bad = True
  if line.find('NO.')+3 > line.find('SAMP')-1:
    if _verbose:
      print>>sys.stderr, str('wctun1: strange 3rd header line'+
                             '(sampling rate, instrument no.)')
    hdr3_bad = True
  if hdr3_bad:
    instrument = '-'
    warned = True
  else:
    all_items['INSTRUMENT NO.'] = line[line.find('NO.')+3:
        line.find('SAMP')-1].strip()
    if len(all_items['INSTRUMENT NO.']) == 0:
      all_items['INSTRUMENT NO.'] = '-'

  if (line.find('RATE') == -1 or
      line.find('HZ')   == -1 or
     (line.find('HZ')-1) - (line.find('RATE')+4) + 1 < 2:
    err = 'unknown error with sampling rate on line 3'
    if line.find('RATE') == -1:
      err = 'no SAMPLING RATE found on line 3'
    elif line.find('HZ') == -1:
      err = 'no HZ found on line 3'
    else:
      err = "can't read sampling rate"
    if _verbose:
      print>>sys.stderr,'wctun1: %s' % err
    all_items['SAMPLING RATE'] = -9
    return WCTUN1_ERR(all_items, 1)

  try:
    unconverted = line[line.find('RATE')+4:line.find('HZ')-1]
    if (unconverted.find('.') != -1 or
        unconverted.find('e') != -1 or
        unconverted.find('E') != -1):
      all_items['SAMPLING RATE'] = float(unconverted)
    else:
      all_items['SAMPLING RATE'] = (int(unconverted[:-2])+
          float(str('.'+unconverted[-2:])))
  except (TypeError, ValueError):
    if _verbose:
      print>>sys.stderr,"wctun1: couldn't convert %s into f7.2" % unconverted
    all_items['SAMPLING RATE'] = -9
    return WCTUN1_ERR(all_items, 1)

  return (all_items, 0)
