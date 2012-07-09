#!/usr/bin/env python

execfile('libwocecvt.py')
execfile('libwctcvt.py')

def wctun2(file_contents,begin,end,nrecords,):
  global _verbose,starred
  global numo,MXCNUM,MXCTD

  all_items = {}
  qualt1_begin = 0
  qualt1_end   = 0
  qualt2_begin = 0
  qualt2_end   = 0

  if begin[-2] != -1:
    scrtch = file_contents[0][begin[-2]:end[-2]].strip()
    if starred != len(scrtch):
      if _verbose:
        print>>sys.stderr,'wctun2: mismatched QUALT1 observation;',
        print>>sys.stderr,'found %d of %d expected'%(len(scrtch),starred)
      return (all_items, 1)
    else:
      qualt1_begin = end[-2]-starred+1
      qualt1_end   = end[-2]

  if begin[-1] != -1:
    scrtch = file_contents[0][begin[-1]:end[-1]].strip()
    if starred != len(scrtch):
      if _verbose:
        print>>sys.stderr,'wctun2: mismatched QUALT2 observation;',
        print>>sys.stderr,'found %d of %d expected'%(len(scrtch),starred)
      return (all_items, 1)
    else:
      qualt2_begin = end[-1]-starred+1
      qualt2_end   = end[-1]

  DATA = []
  for param in numo:
    DATA.append([])

  i = 1
  for line in file_contents[1:]:
    if qualt1_begin > -1:
      all_items['QUALT1'].append(line[qualt1_begin:qualt1_end])
    if qualt2_begin > -1:
      all_items['QUALT2'].append(line[qualt2_begin:qualt2_end])
    for i in range(MXCNUM): # FIXME
      DATA[i].append(line[i*8:i*8+8])
    i += 1
    if i > nrecords:
      if _verbose:
        print>>sys.stderr,'wctun2: unexpected continuation of file;',
        print>>sys.stderr,'please check nrecords (%d)'%nrecords
      return (all_items, 1)

  all_items['DATA'] = {}
  for i in range(len(DATA)):
    all_items[numo[i]] = DATA[i]

  return (all_items, 0)
