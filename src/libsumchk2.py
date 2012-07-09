#!/usr/bin/env python
import optparse
import sys

options = None

def set_max_error(option, opt_str, value, parser):
  setattr(parser.values, option.dest, value)
  parser.values.max_error_set = True

optparser = optparse.OptionParser('usage: %prog [options] FILE ...')
optparser.values.max_error_set = False
optparser.add_option('-q', '--quiet',
                       action='store_false',
                       dest='verbose',
                       default=True,
                       help='suppress diagnostic messages')
optparser.add_option('-e', '--max-error',
                       action='callback',
                       callback=set_max_error,
                       type='int',
                       dest='max_error',
                       default=20,
                       help='set maximum allowed errors (--robust override)')
optparser.add_option('-r', '--robust', '--dont-die',
                       action='store_true',
                       dest='robust',
                       default=False,
                       help='continue checking regardless of errors')

def say(msg):
  if options.verbose:
    print >> sys.stderr, msg
