#!/usr/bin/env python
from re import match, finditer
''' sumlim.py
Gets column spans for a WOCE summary file.
    initialize_correctedness_indicators(7)
        Sets up depth limits for checking later.
    get_summary_limits(1)
        Discovers and yields a table of column spans for each parameter in
        a WOCE summary file.
'''

execfile('libsumchk.py')

# convenience constants
DEPTH1 = DEPTH_UNC
DEPTH2 = CDEPTH

def initialize_correctedness_indicators(marker,
                                        header2,
                                        dep_bg,
                                        begin,
                                        corrected,
                                        duplicate_message,
                                        assumption_message,):
  '''Sets up depth usage for full check.
     marker                tag that identifies depth column type
     header2               header to search in
     dep_bg                depth column limits (beginning).
     begin                 column limits (beginning).
     corrected             Whether to initialize for corrected depth.
     duplicate_message     Message for two columns of same type.
     assumption_message    Message for assuming a value is (un)corrected.
     (no return value)
  '''
  global _perr
  cor = [False, False] # truth table for header matches
  cor[0]=(match(marker,header2[dep_bg[0]:dep_bg[0]+len(marker)])is not None)
  cor[1]=(match(marker,header2[dep_bg[1]:dep_bg[1]+len(marker)])is not None)

  if cor[0] and cor[1]: # conflicting depth columns
    if _perr:
      print >> sys.stderr, duplicate_message, "will use first."
    if corrected:
      begin[DEPTH2] = dep_bg[0]
      begin[DEPTH1] = -1
    else:
      begin[DEPTH1] = dep_bg[0]
      begin[DEPTH2] = -1

  elif cor[0]:
    if corrected:
      begin[DEPTH2] = dep_bg[0]
      if dep_bg[1] != -1:
        begin[DEPTH1] = dep_bg[1]
    else:
      begin[DEPTH1] = dep_bg[0]
      if dep_bg[1] != -1:
        print >> sys.stderr, assumption_message % dep_bg[1]
        begin[DEPTH2] = dep_bg[1]

  else: # cor[1]
    if corrected:
      begin[DEPTH2] = dep_bg[1]
      if dep_bg[0] != -1:
        begin[DEPTH1] = dep_bg[0]
    else:
      begin[DEPTH1] = dep_bg[1]
      if dep_bg[0] != -1:
        print >> sys.stderr, assumption_message % dep_bg[0]
        begin[DEPTH2] = dep_bg[0]

def get_summary_limits(file_contents,):
  '''Gets column spans for parameters.
     file_contents     The contents of the summary file.
     returns a 2-tuple containing the array of beginning and end
     column indices in the file for each parameter.
  '''
  global _perr, misplaced
  # read header lines
  try:
    header2 = file_contents[0]
    del file_contents[0]
    header3 = file_contents[0]
    del file_contents[0]
  except IndexError:
    print >> sys.stderr, 'encountered empty file!'
    return (None, None)

  # validate header2
  if header2.find('POS') == -1:
    if header2.find('osit') == -1:
      if _perr:
        print >> sys.stderr, str("sumlim: couldn't recognize"+
              "2nd header, but will try to continue.")
    else:
      if _perr:
        print >> sys.stderr, "sumlim: 2nd header has lower case."

  # validate header3
  if header3.find('EXPO') == -1:
    if _perr:
      print >> sys.stderr, str("sumlim: expecting 3rd header. looking"+
               "for 'EXPO'. got ="), header3
    return (None, None)

  # extract character indices where columns begin
  begin = []
  end   = []
  for i in HEADINGS.keys():
    heading = HEADINGS[i][0]
    if HEADINGS[i][1] != 0:
      begin.append(header3.find(heading[:HEADINGS[i][1]]))
    else:
      begin.append(-1)
  if begin[7] != -1:
    begin[7] += 1 # FIXME for some strange reason

  # determine WIRE_OUT separately
  if begin[WHEEL_MT] == -1:
    begin[WHEEL_MT]=header2.find('WIRE')

  # determine depth column limits separately
  dep_bg = []
  dep_bg.append(header3.find(
      HEADINGS[DEPTH1][0][:HEADINGS[DEPTH1][1]]))
  if dep_bg[0] == -1:
    dep_bg.append(-1)
  else:
    dep_bg.append(header3[dep_bg[0]:].find(
        HEADINGS[DEPTH2][0][:HEADINGS[DEPTH2][1]]))
    if dep_bg[1] != -1: # adjust to absolute character column
      dep_bg[1] += dep_bg[0]

  # figure out:
  # - which column, if any, is corrected (make assumptions if necessary)
  # - which column to use (corrected if possible)
  cor = [False, False]
  begin[DEPTH1] = -1
  begin[DEPTH2] = -1
  # no depth columns
  if dep_bg[0] == -1 and dep_bg[1] == -1:
    pass
  # explicit corrected depth
  elif (match('CDEP', header3[dep_bg[0]:dep_bg[0]+4]) or
       match('CDEP', header3[dep_bg[1]:dep_bg[1]+4])):
    cor = initialize_correctedness_indicators('CDEP', header2, dep_bg, begin,
              True, 'found 2 corrected depth cols.', '')
  # implicit corrected depth from header2
  elif (match('COR', header2[dep_bg[0]:dep_bg[0]+3]) or
       match('COR', header2[dep_bg[1]:dep_bg[1]+3])):
    cor = initialize_correctedness_indicators('COR', header2, dep_bg, begin,
              True, 'found 2 corrected depth cols.', '')
  # implicit uncorrected depth from header2
  elif (match('UNC', header2[dep_bg[0]:dep_bg[0]+3]) or
       match('UNC', header2[dep_bg[0]:dep_bg[0]+3])):
    cor = initialize_correctedness_indicators('UNC', header2, dep_bg, begin,
              False, 'found 2 uncorrected depth cols.',
              "asssuming depth at col:%d is corrected depth.")
  # no explicit or implicit correction
  else:
    if dep_bg[0] != 0 and dep_bg[1] != 0:
      if _perr:
        print >> sys.stderr, 'two depth cols found. assuming 2nd is corrected.'
      begin[DEPTH1] = dep_bg[0]
      begin[DEPTH2] = dep_bg[1]
    elif dep_bg[0] != 0:
      if _perr:
        print >> sys.stderr, 'assuming depth is corrected.'
      begin[DEPTH1] = 0
      begin[DEPTH2] = dep_bg[0]
    elif dep_bg[1] != 0:
      if _perr:
        print >> sys.stderr, 'assuming depth is corrected.'
      begin[DEPTH1] = 0
      begin[DEPTH2] = dep_bg[1]
    else:
      if _perr:
        print >> sys.stderr, str('An error occurred during column processing.'+
                           'Please notify cchdo@ucsd.edu')

  # list missing parameters
  j = 0
  for i in HEADINGS.keys():
    if begin[i] == -1:
      j += 1
      if j == 1:
        if _perr:
          print >> sys.stderr, ' .SUM header was missing:'
      if _perr:
        print HEADINGS[i][0],
  if _perr:
    print

  # make sure COMMENTS are last; abort if not so
  if begin[COMMENTS] != -1:
    for i in HEADINGS.keys():
      if begin[i] > begin[COMMENTS]:
        if _perr:
          print >> sys.stderr, 'sumlim: COMMENT parameter is not last.'
        misplaced.append(0)
        return (None, None)

  # find header endings
  for i in range(MXHDNG):
    end.append(199)
  for i in range(len(begin)):
    if begin[i] == -1:
      continue
    else:
      j = i + 1
      while j < len(begin) - 1 and begin[j] == -1:
        j += 1
      if j < len(begin):
        end[i] = begin[j] - 1
      else:
        end[i] = 199

  # show the column limits
# for i in range(len(begin)):
#   print "i=%2d: bg=%9d    en = %9d" % (i+1, begin[i]+1, end[i]+1)

  return (begin, end)

