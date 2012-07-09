#!/usr/bin/env python

execfile('libwocecvt.py')
execfile('libwctcvt.py')

def cddcfu(begin, names, units,):
  global starred, nparams
  global MXCTD, MXCNUM
  h6, h7 = '', ''
  h6 = '%-6s  %-6s  %-6s' % ('CTDPRS','CTDTMP','CTDSAL')
  h7 = '%-8s%-8s' % (units[0].strip(),units[1].strip())

  if begin[3] == -1:
    pass
  else:
    h6 += '%-6s' % 'CTDOXY'
    h7 += '%-8s' % units[3].strip()

  for i in range(5, MXCNUM):
    if i == 7:
      continue # for some strange reason
    else:
      h6 += '%-8s' % names[i].strip()
      h6 += '%-8s' % units[i].strip()

  h6 += 'QUAL'

  return (h6, h7)
