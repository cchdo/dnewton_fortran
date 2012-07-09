#!/usr/bin/env python

execfile('libwocecvt.py')
execfile('libwctcvt.py')

def formatof(n):
  formats = ['%7.1f ', '%7.4f ', '%7.4f ',
             '%7.1f ', '%7.2f ', '%7.3f ',
             '%7.0f ', '%7.4f ', '%7.2f ',]
  return formats[n]

def cddcfd(begin, qualps, nobs, dat, qualt,): # qualt=qulmod
  global MXCTD, MXCNUM, MXERR
  global _verbose, numo
  qtab   = [[-1,1,1,1,1,0,1,1,-1,0,],
            [-1,1,1,0,0,0,2,2,-1,0,],
            [-1,1,1,3,0,0,2,2,-1,0,],]
  bigformat = ''
  for param in dat:
    bigformat += formatof(numo.index(param))
  bigformat += '%8d'

  nerrs = 0
  warn = False
  contents = []
  for i in range(nobs):
    q = []
    tdata = map((lambda x: x[i]), dat)
    for j in range(len(dat)):
      if j == 0:
        continue # for some strange reason
      c1 = 0
      if qualps[j] == 0:
        c1 = 2
      else:
        try:
          c1 = int(qualt[i][qualps[j]])
          q.append(qtab[c1][qualt])
        except (TypeError, ValueError):
          if _verbose:
            print>>sys.stderr,"cddcfd: can't convert",
            print>>sys.stderr,"%s to int"%qualt[i][qualps[j]]
          c1 = 0
          q.append(-1)
      if c1 == 9 and tdata[j] > -9:
        if _verbose:
          print>>sys.stderr,"cddcfd: (line %d; press=%f)"%(i,tdata[0]),
          print>>sys.stderr,"flag shows missing but value isn't"
        warn = True
        q.append(0)
      if q[j] > 0 and tdata[j] <= -9:
        if _verbose:
          print>>sys.stderr,"cddcfd: (line %d; press=%f)"%(i,tdata[0]),
          print>>sys.stderr,"value shows missing, but flag isn't"
        warn = True
        q.append(0)
      if q[-1] == -1:
        q[-1] = 0
        tdata[j] = -9
        if _verbose:
          print>>sys.stderr,'cddcfd: bad quality flag %d'%c1,
          print>>sys.stderr,'on line %d, press=%f'%(i,tdata[0])
        nerrs += 1
        warn = True
      elif q[-1] == 0:
        tdata[j] = -9
    for j in q[1:]:
      tdata.append(j)
    contents.append(bigformat%(tdata))
    if nerrs > MXERR:
      if _verbose:
        print>>sys.stderr,'cddcfd: max errs reached; aborting cast'
        warn = True
        return (contents, warn, 1)
  return (contents, warn, 0)
