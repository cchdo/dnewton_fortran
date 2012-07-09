#!/usr/bin/env python

execfile('libwocecvt.py')
execfile('libwctcvt.py')

execfile('sumqwk.py')
execfile('sumun1.py')

first_call = True

def find_sum_line(file_contents,begin,end,stn,cast,):
  global MXHDNG, MAX_SUMMARY_LINES, MXINDEX
  global _verbose, first_call

  all_stations = []
  line         = None
  expo         = None

  if first_call:
    first_call   = False
    first_sumqwk = True
    prev_stn     = ' '

    if file_contents[3].find('---') == -1:
      if _verbose:
        print>>sys.stderr,'fndsum: expected "---" on line 4 of .SUM file.'
      return (line, expo, 2)

    if _verbose and len(file_contents) - 5 > MAX_SUMMARY_LINES:
      print>>sys.stderr,'fndsum: warning: too many lines in .SUM file'

    for i in range(4,len(file_contents)):
      blob = sumqwk(file_contents[i],begin,end,first_sumqwk)
      expocode, stnnbr, castno, ctype, tcode, _ = blob
      if _ < 0:
        continue
      elif _ > 0:
        if _verbose:
          print>>sys.stderr,'fndsum: error in sumqwk; aborting current'
        return (line, expo, _)

      strict = True # FIXME optionable?
      if strict:
        all_items, _ = sumun1(file_contents[i],begin,end)
        if _:
          if _verbose:
            print>>sys.stderr,'fndsum: warning from sumun1 for line',i
        if stnnbr != prev_stn:
          prev_stn = stnnbr
          i1 = -9999
          try:
            i1 = int(stnnbr.replace(' ', ''))
          except (TypeError, ValueError):
            pass
          all_stations.append( (i, stnnbr, i1) )
          if len(all_stations) > MXINDEX:
            if _verbose:
              print>>sys.stderr,'fndsum: too many stations to index',
              print>>sys.stderr,'(max %d)' % MXINDEX
            return (line, expo, 2)

  wanted_stn = -9999
  try:
    wanted_stn = int(stn.replace(' ',''))
  except (TypeError, ValueError):
    pass
  k = None

  for i in range(len(all_stations)):
    if ((wanted_stn == -9999 and stn == all_stations[i][1]) or
        (all_stations[i][2] > -9998 and wanted_stn == all_stations[i][2])):
      k = i
      break
  if k is None:
    if _verbose:
      print>>sys.stderr,'fndsum: no STNNBR %s in .SUM file' % stn
    return (line, expo, 1)

  if k < 0 or k >= MXINDEX:
    print>>sys.stderr,'fndsum: error finding requested station; at',k
    return (line, expo, 99)

  if all_stations[k+1][0] - all_stations[k][0] > MAX_LINES:
    if _verbose:
      print>>sys.stderr,'fndsum: too many lines for station %s' % stn
    return (line, expo, 2)

  all_lines = []
  for i in range(all_stations[k][0], all_stations[k+1][0]):
    blob, _ = sumqwk(file_contents[i],begin,end,first_sumqwk)
    expocode, stnnbr, castno, ctype, tcode, _ = blob
    if _ == -1:
      continue
    if castno == cast:
      all_lines.append(file_contents[i])

  if len(all_lines) == 0:
    if _verbose:
      print>>sys.stderr,'fndsum: found station %s'%stn,
      print>>sys.stderr,'but no cast %s'%cast
    return (line, expo, 3)

  elif len(all_lines) == 1:
    expo,stnnbr,castno,ctype,tcode,_ = sumqwk(
        all_lines[0],begin,end,first_sumqwk)

  else:
    best_line  = None
    best_score = 0
    best_warnp = False
    best_expo  = None
    for line in all_lines:
      score = 0
      all_items, _ = sumun1(line,begin,end)
      ####TODO:232
      if all_items['BOTMUN'] > -8:
        score += 1
      if all_items['BOTMCO'] > -8:
        score += 1
      if all_items['HAB'] > -8:
        score += 1
      if all_items['WHEEL'] > -8:
        score += 1
      if all_items['MXPRES'] > -8:
        score += 1
      if all_items['NBOTS'] > -8:
        score += 1
      if len(all_items['PARAMS'].strip()) > 0:
        score += 1
      if len(all_items['COMMENTS'].strip()) > 0:
        score += 1
      if score > best_score:
        best_line  = line[:]
        best_score = score
        best_warnp = (_!=0)
        best_expo  = all_items['EXPOCODE']

    if _verbose and best_warnp:
      print>>sys.stderr,'fndsum: warning: stn=%s cast=%s'%(stn,cast),
      print>>sys.stderr,"produced error in ``best line''"
    line = best_line
    expo = best_expo

  return (line, expo, 0)
