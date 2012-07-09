#!/usr/bin/env python
from __future__ import with_statement
import sys

execfile('libsumchk.py')
execfile('sumlim.py')
execfile('sumqwk.py')
execfile('sumun1.py')

'''
check(filename)
Wrapper function for checking a WOCE summary file (*.sum).
NB. Only applies to older versions of WOCE summary file. Do not attempt to
use this checker to verify integrity of newer (*su.txt) files.
(2010-03-30-AS) Made some changes over last week to improve compatibility
with newer *su.txt files.

filename        name of WOCE summary file to check
(no return type)
'''
def check(filename):
  global MAX_ERROR, _max_error_set, MAX_PROCESSABLE_LINES, _die_on_too_many_errors, _perr
  global read_err, current_line
  file_contents = [] # buffer of file contents by line
  begin         = [] # indices of column beginnings
  end           = [] # indices of column endings

  # read summary file into buffer
  with open(filename, 'r') as file:
    for line in file:
      file_contents.append(line)

  # discard main header
  del file_contents[0]

  # get column limits from header lines
  begin, end = get_summary_limits(file_contents)
  if begin is None and end is None:
    read_err.append(0)
    return 1
 # if _perr:
 #   print #
  # skip first line (?)
  # (AS) this was in the original source. assuming it's important.
  del file_contents[0]

  firstp   = True # whether first call to sumqwk()
  nlines   = 0    # number of lines in file
  warnings = 0    # total warnings in file

  # warn for too many lines in file (obsolete)
  if len(file_contents) > MAX_PROCESSABLE_LINES:
    print >> sys.stderr, str("main: (warning) too many lines in"+
                             " %s; continuing...") % filename

  for i in range(len(file_contents)):
    line = file_contents[i]
    current_line = i
    # get identifying parameters
    # TODO specify uniform interface for sumqwk/sumun1:
    #      return values in hashes
    expocode,stnnbr,castno,ctype,tcode,_ = sumqwk(line,begin,end,firstp)
    if _ != 0:
      if _ == -1:
        continue # ignore read errors
      else:
        # report invalid column widths
        if _perr:
          read_err.append(i)
          print >> sys.stderr, "main: decode error in sumqwk. rec=%d" % i

    # record that this line has been checked
    nlines += 1

    # get all other parameters
    all_items, did_warn = sumun1(line, begin, end)

    if did_warn:
      #print >> sys.stderr, "(main: warning -- line %d)" % i
      warnings += 1

      # stop checking if too many errors occurred
      # unless told not to die
      if warnings > MAX_ERROR and _die_on_too_many_errors:
        print >> sys.stderr, "main: too many warnings. abort."
        break

  # show check results (summarized)
  if _perr:
    print "data lines examined=%d" % nlines
    print "check completed. number of lines with warnings: %d" % warnings

  # return number of warnings
  return warnings

'''
main()
Wrapper for checker in general. Handles arguments and iterates over filenames
to check().
'''
def main():
  global MAX_ERROR, _max_error_set, MAX_PROCESSABLE_LINES, _die_on_too_many_errors, _perr
  global _illegal_no_data_dash
  i              = 1     # index of current argument; start after prgm call
  files          = []    # list of filenames to parse
  _max_error_set = False # whether max errors was set

  while i < len(sys.argv):
    # '-h', '--help': print a usage message
    if sys.argv[i] in ['-h', '--help', '-?']:
      print '''\tsumchk2-py (beta)

    usage: [./]sumchk2[.py] [(-h|-?|--help)]
                            [(-e|--max-error) MAX_ERROR]
                            [(-r|--robust|--dont-die)]
                            [(-q|--quiet)]
                            [FILES...]
Python translation of original Fortran WOCE summary checker.
You will be prompted for the name of the summary file and the
maximum allowed errors.'''
      exit(0)

    # '-e', '--max-error': set maximum number of errors
    elif sys.argv[i] in ['-e', '--max-error']:
      i += 1 # get next arg as number
      if i >= len(sys.argv): # expected a number, got nothing (end of args)
        print >> sys.stderr, "missing argument to `--max-error'"
        exit(1)
      try: # try to parse arg as number of errors allowed
        MAX_ERROR = int(sys.argv[i])
        _max_error_set = True
      except TypeError: # arg not a number; default to 20 errors
        print >> sys.stderr, "`%s' not int; defaulting 20" % sys.argv[i]
        MAX_ERROR = 20
        _max_error_set = False # and prompt user for a number

    # '-r', '--robust', '--dont-die': do not stop checking if there
    # are too many errors
    elif sys.argv[i] in ['-r', '--robust', '--dont-die']:
      _die_on_too_many_errors = False
      _max_error_set = True # MAX_ERROR = Infinity

    elif sys.argv[i] in ['-q', '--quiet']:
      _perr = False

    else:
      # assume argument is a filename
      files.append(sys.argv[i])

    i += 1 # next argument

  if len(files) == 0:
    # get filename from user
    print "\t%s*** SUMCHK2 BETA 2010-03-30 ***%s\n" % (BOLD, CLEAR)
    print " program sumchk verifies woce .SUM files."
    print " enter input .SUM filename:"
    files.append(raw_input())

  if not _max_error_set:
    # get max errors from user
    print " enter maximum number of errors:"
    MAX_ERROR = None
    while MAX_ERROR is None:
      try:
        MAX_ERROR = int(raw_input())
      except TypeError:
        print >> sys.stderr, " invalid number (must =~ /[0-9]+/)."

  for filename in files:
    n = check(filename)
    if n > 0:
      print filename, 'failed with %d error(s)' % n
      if _illegal_no_data_dash:
        print " - uses '-' to mark (NO DATA) -- please fix immediately"
      if len(read_err) > 0:
        print ' - %3d read error(s)' % len(read_err)
      if len(overlarge) > 0:
        print ' - %3d overlarge parameter(s)' % len(overlarge)
      if len(misplaced) > 0:
        print ' - misplaced comment column'
      if len(missing) > 0:
        print ' - (warning) %3d missing value(s)' % len(missing)
      if len(unknown) > 0:
        print ' - %3d unknown parameter(s)' % len(unknown)
      if len(strange) > 0:
        print ' - %3d strange parameter(s)' % len(strange)
    else:
      print >> sys.stderr, filename, 'ok'

main()
