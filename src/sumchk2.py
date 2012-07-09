#!/usr/bin/env python
from __future__ import with_statement
#import optparse
#import sys
import libsumchk2 # TODO

def sumchk(filename):
  BOUNDS_ERROR = 1
  SPACE = ' \f\n\r\t\v'

  line2 = ''
  line3 = ''
  with open(filename, 'rb') as file:
    file.readline()
    line2  = file.readline().strip(SPACE)
    line3  = file.readline().strip(SPACE)
    bounds = sumlim2.limits_of(line2, line3)
    if bounds is None:
      return BOUNDS_ERROR
    file.readline() # ???
    line_number = 5
    for line in file:
      identifiers, _ = quickread(line, bounds)
      if _ != 0:
        print >> sys.stderr, 'error handle'
      line_number += 1

def main():
  libsumchk2.options, args = libsumchk2.optparser.parse_args()

  if len(args) < 1:
    #libsumchk2.optparser.error('sumchk2: no input files')
    print " program sumchk verifies woce .SUM files."
    print " enter input .SUM filename:"
    args.append(raw_input())

  if not libsumchk2.options.values.max_error_set:
    print " enter maximum number of errors:"
    libsumchk2.options.values.max_error = None
    while libsumchk2.options.values.max_error is None:
      try:
        libsumch2.options.values.max_error = int(raw_input())
      except (TypeError, ValueError):
        print >> sys.stderr, " invalid number (must =! /[0-9]+/)."

  for filename in args:
    sumchk(filename)

if __name__ == '__main__':
  pass
  #main()
