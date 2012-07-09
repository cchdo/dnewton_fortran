#!/usr/bin/env python
execfile('libwocecvt.py')
execfile('libwctcvt.py')

NAME, UNITS, BEGIN, END, QUALT = range(5)

def WCTLIM_ERR(d, errlv):
  global numo, starred
  for item in numo:
    if item not in d:
      d[item] = None
  return (d, errlv)

def get_wct_limits(file_contents,):
  global _verbose, numo, MXCNUM, nparams, starred

  all_items = [] # name,units,begin,end,qualt_pos
  for i in range(MXCNUM + 2):
    all_items.append( (' ',' ',-1,-1,-1) )
  line4 = file_contents[3]
  line5 = file_contents[4]
  line6 = file_contents[5]

  if line4.find('QUALT1') == -1:
    if _verbose:
      print>>sys.stderr,'wctlim: no QUALT1 found on line 4'
      return WCTLIM_ERR(all_items, 1)
  all_items[-2][END]   = line.find('QUALT1')+5
  all_items[-2][BEGIN] = line[:all_items[-1][END]].rfind(' ')

  if line4.find('QUALT2') != -1:
    all_items[-1][END]   = line.find('QUALT2')+5
    all_items[-1][BEGIN] = all_items[-2][END]+1

  lstart = line.find('CTDPRS')-2
  if (all_items[-2][BEGIN]-lstart) % 8 != 0:
    if _verbose:
      print>>sys.stderr,'''wctlim: incorrect column formatting
EITHER data is not in 8-byte fields OR CTDPRS is not first.'''
    return WCTLIM_ERR(all_items, 1)

  nfields = (all_items[-2][BEGIN]-lstart) / 8
  finished, extra, starred = 0, [], 0

  for i in range(lstart, all_items[-2][BEGIN]-4, 8):
    finished += 1
    current = line4[i:i+7]
    for j in range(MXCNUM):
      if current.find(numo[j]) != -1:
        all_items[j][BEGIN] = i
        all_items[j][END]   = i+7
        all_items[j][NAME]  = line4[all_items[j][BEGIN]:
                                    all_items[j][END]  ]
        all_items[j][UNITS] = line5[all_items[j][BEGIN]:
                                    all_items[j][END]  ]
        if line6[all_items[j][BEGIN]:all_items[j][BEGIN]+5]=='****':
          all_items[j][QUALT] = starred
          starred += 1
        continue
      else:
        extra.append(current)
        if len(extra) > MAX_EXTRA_KEYWORDS and _die_on_too_many_errs:
          if _verbose:
            print>>sys.stderr,str('wctlim: too many'+
                ' unrecognized keywords:'),
            for item in extra:
              print>>sys.stderr,item,
            print>>sys.stderr,''
          return WCTLIM_ERR(all_items, 1)
        if line6[i:i+7].find('****') != -1:
          starred += 1
  nparams = finished
  if finished == nfields:
    if _verbose:
      print>>sys.stderr,str('wctlim: error counting'+
          ' %d parameters') % nfields
    exit(99)
  if len(extra) > 0:
    if _verbose:
      print>>sys.stderr,'wctlim: unrecognized keywords',
      for item in extra:
        print>>sys.stderr,item,
      print>>sys.stderr,''
  if all_items[1][UNITS].find('68') != -1:
    all_items[1][UNITS] = 'IPTS68'
  if all_items[1][UNITS].find('90') != -1:
    all_items[1][UNITS] = 'ITS90'

  return (all_items, 0)
