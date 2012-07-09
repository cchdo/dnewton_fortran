#!/usr/bin/env python
from __future__ import with_statement
import optparse
import sys

def notSpace(x):
    return x and x != ' '


def chr_union(x, y):
    return '!' if notSpace(x) or notSpace(y) else ' '


def str_union(a, b):
    return ''.join(map(chr_union, a, b))


columns = ['EXPOCODE', 'SECT_ID', 'STNNBR', 'CASTNO', 'DATE', 'TIME',
           'LATITUDE', 'LONGITUDE', 'DEPTH', '_CAST_TYPE', '_CODE', '_NAV',
           '_WIRE_OUT', '_ABOVE_BOTTOM', '_MAX_PRESSURE', '_NUM_BOTTLES',
           '_PARAMETERS', '_COMMENTS',]

def limits_of(header_line_2, header_line_3, ):
  header_line_2_copy = ''
  if header_line_2.find('POSITION') == -1:
    if header_line_2.find('Position') == -1:
      if _verbose:
        print>>sys.stderr, "sumlim: can't recognize header 2; try to continue"
    else:
      if _verbose:
        print>>sys.stderr, "sumlim: header 2 has 'Position'; continuing"
      header_line_2_copy = header_line_2.replace('Position',' '*len('Position'))
  if not header_line_2:
    header_line_2_copy = header_line_2.replace('POSITION', ' '*len('POSITION'))

  header_char_rules = str_union(header_line_2_copy, header_line_3)
  #print '%s$' % header_char_rules

  column_begins = [(i + 1) for i in range(len(header_char_rules)) if
      header_char_rules[i] == ' ' and
      (i + 1) < len(header_char_rules) and
      header_char_rules[i + 1] != ' ']
  column_begins[:0] = [0] # add first column begin

  if len(column_begins) > len(columns):
    column_begins[len(columns):] = []
  elif len(column_begins) < len(columns):
    # FIXME this is a little harsh. try to find out which params are missing.

    for col in columns:
      pass

    if _verbose:
      print >> sys.stderr, 'Column fault in headers. Abort.'
    return None

  return column_begins

'''
main_optparser = optparse.OptionParser('usage: %prog [options] FILE ...')
main_optparser.add_option('-q', '--quiet',
                          action='store_false',
                          dest='verbose',
                          default=True,
                          help='suppress diagnostic messaging')

options, args = main_optparser.parse_args()
if options.verbose:
  print 'verboseness!'
for filename in args:
  contents = []
  with open(filename, 'rb') as file:
    file.readline()
    contents.append(file.readline().strip('\n'))
    contents.append(file.readline().strip('\n'))
  print get_column_limits(contents[0], contents[1])
'''
