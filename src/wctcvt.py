#!/usr/bin/env python
from __future__ import with_statement
import sys

execfile('libwocecvt.py')
execfile('libwctcvt.py')
execfile('sumlim.py')

load_carter_table_file(CARTERTAB_ATB)
load_carter_table_file(CARTERTAB_TB)

def convert(sum_filename,):
  global QCODE_DQE, QCODE_PICKY, QCODE_ARCHIVE
  global USE_QUALT1, USE_QUALT2
  global use_carter_tables, qcode_mode, qualt
  global wct_filename, out_filename

  sum = []
  with open(sumfile, 'r') as sumf:
    for line in sumf:
      sum.append(line)

  del sum[0]
  begin, end = get_summary_limits(sum)
  if begin is None and end is None:
    return 1

  if begin[CDEPTH] != -1:
    pass # use available corrected depths.
  elif use_carter_tables:
    if _verbose:
      print ' bottom depths will be corrected with carter tables.'
  else:
    if _verbose:
      print ' no corrected bottom depths; no conversion.'

  wct = []
  with open(wctfile, 'r') as wctf:
    for line in wctf:
      wct.append(line)

  nstn = 0
  same_expocode_p = True

  if len(wct) == 0:
    if _verbose:
      print "%3d station/casts converted" % nstn
    return 0
  for line in wct:
    lst = []
    with open(line, 'r') as lstfile:
      for line in lstfile:
        list.append(line)

    identifiers, _ = wctun1(lst)
    if _ > 0:
      if _ == 1:
        pass
      else:
        if _verbose:
          print>>sys.stderr,'wctcvt: error in wctun1; aborting current'
        continue # to next list file

    headers, _ = wctlim(list)
    if _ > 0:
      if _ == 1:
        pass
      else:
        if _verbose:
          print>>sys.stderr,'wctcvt: error in wctlim; aborting current'
        continue # to next file
    name,units,begin,end,qualt_pos = headers

    all_items, _ = wctun2(list[6:],begin,end,identifiers['NO. RECORDS'])
    if _ > 0:
      if _ == 1:
        if _verbose:
          print>>sys.stderr,'wctcvt: warning from wctun2 for %s' % line
      else:
        if _verbose:
          print>>sys.stderr,'wctcvt: error in wctun2; aborting current'
        continue # to next list file

    previous = -999
    for pressure in all_items['DATA']['CTDPRS']:
      if pressure <= previous:
        if _verbose:
          print>>sys.stderr,'wctcvt: pressures do not increase',
          print>>sys.stderr,'monotonically; aborting current'
        previous = -999
        break # to handler
      else:
        previous = pressure
    if previous == -999: # handler
      continue # to next file

    line,expocode,_ = find_sum_line(sum,begin,end,stnnbr,castno)
    if _ != 0:
      if _ == 3:
        if _verbose:
          print>>sys.stderr,'wctcvt: cast %s not found;',
          print>>sys.stderr,'trying 1'%str(castno)
        line,expocode,_ = find_sum_line(sum,begin,end,stnnbr,1)
        if _ != 0:
          if _verbose:
            print>>sys.stderr,'wctcvt: cast 1 not found'
            print>>sys.stderr,'wctcvt: skipping stn=%s'%str(stnnbr),
            print>>sys.stderr,'cast=%s'%str(castno)
          continue # to next file
        else:
          if _verbose:
            print>>sys.stderr,'wctcvt: using cast 1',
            print>>sys.stderr,'for stn=%s'%str(stnnbr)
      elif _ == 1:
        if _verbose:
          print>>sys.stderr,'wctcvt: station %s not found'%str(stnnbr)
        continue # to next file

    if expocode.find('/') == -1:
      same_expocode_p = (expocode == identifiers['EXPOCODE'])
    else:
      same_expocode_p = (expocode[:expocode.find('/')] ==
                         identifiers['EXPOCODE']
                             [:identifiers['EXPOCODE'].find('/')])

    headers = cddcfh(line, begin, end, identifiers['DATE']['year'],
              identifiers['DATE']['month'], identifiers['DATE']['day'],
              identifiers['NO. RECORDS'], identifiers['INSTRUMENT'])
    headers[4]=(headers[4][:35]+'%7.1f'%all_items['DATA']['CTDPRS'][1]+
                headers[4][41:])
    headers[5]=(headers[5][:35]+'%7.1f'%all_items['DATA']['CTDPRS'][-1]+
                headers[5][41:])

    headers[6], header[7], h67len = cddcfu(
        map((lambda x: x[2]), headers),
        map((lambda x: x[0]), headers),
        map((lambda x: x[1]), headers))

    contents,warn,_ = cddcfd(begin, qualt_pos, identifiers['NO. RECORDS'],
                         all_items['DATA'], all_items['QUALT%1s'%qualt],
    if _ != 0:
      if _verbose:
        print>>sys.stderr,'wctcvt: error in stn=%s cast=%s'%(
                          str(stnnbr),str(castno))
    if warn:
      if _verbose:
        print>>sys.stderr,'wctcvt: warning in stn=%s cast=%s'%(
                          str(stnnbr),str(castno))
    with open(out_filename, 'a') as of:
      for line in contents:
        of.write(line)

def main():
  global qcode_mode, _qcode_mode_set, _qcode
  global qualt, _qualt_set, ocntry
  global _verbose, sum_filename, out_filename, wct_filename
  i=0
  while i<len(sys.argv):
    if sys.argv[i] in ['-h', '--help']:
      print >> sys.stderr, HELP_MSG
      exit(0)
    elif sys.argv[i] in ['-dqe', '-picky', '-archive']:
      if _verbose:
        print >> sys.stderr, 'using %s mode.' % sys.argv[i][1:]
      qcode_mode = _qcode[sys.argv[i]]
      _qcode_mode_set = True
    elif sys.argv[i] in ['-qualt1', '-qualt2']:
      if _verbose:
        print >> sys.stderr, 'using QUALT%s' % sys.argv[i][-1]
      qualt = int(sys.argv[i][-1])
      _qualt_set = True
    elif sys.argv[i] in ['-o', '--output-file']:
      i += 1
      if i >= len(sys.argv):
        print >> sys.stderr, "wctcvt: missing argument to --output-file"
        exit(1)
      out_filename = sys.argv[i]
    elif sys.argv[i] in ['-l', '--list-file']:
      i += 1
      if i >= len(sys.argv):
        print >> sys.stderr, "wctcvt: missing argument to --list-file"
        exit(1)
      wct_filename = sys.argv[i]
    elif sys.argv[i] in ['-c', '--country']:
      i += 1
      if i >= len(sys.argv):
        print >> sys.stderr, "wctcvt: missing argument to --country"
        exit(1)
      ocntry = sys.argv[i][:2]
    elif sys.argv[i] in ['-q', '--quiet']:
      _verbose = False
    else:
      sum_filename.append(sys.argv[i])
    i+=1
  if (len(sum_filename)==0 or 
      wct_filename is None or
      not _qcode_mode_set  or
      not _qualt_set       or
      out_filename is None):
    print PRGM_HDR
  if len(sum_filename) == 0:
    print ' enter input .SUM filename:'
    sum_filename.append(raw_input())
  if wct_filename is None:
    print ' enter a file containing all the .wct filenames:'
    wct_filename = raw_input()
  if not _qcode_mode_set:
    print QCODE_DESC
    while not _qcode_mode_set:
      print ' choose a mode (1-3):'
      try:
        qcode_mode = int(raw_input())
        _qcode_mode_set = (qcode_mode in range(1,4))
      except (TypeError, ValueError):
        _qcode_mode_set = False
  if not _qualt_set:
    while not _qualt_set:
      print 'Look at QUALT1 or QUALT2? (1 or 2):'
      try:
        qualt = int(raw_input)
        _qualt_set = (qualt in range(1,3))
      except (TypeError, ValueError):
        _qualt_set = False
  if out_filename is None:
    print ' enter output filename:'
    out_filename = raw_input()
