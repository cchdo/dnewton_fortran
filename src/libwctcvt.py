#!/usr/bin/env python
from __future__ import with_statement

numo = [' CTDPRS',' CTDTMP',' CTDSAL',' CTDOXY',
        '  XMISS','  FLUOR',' NUMBER','CTDCOND',
        '  SOUND',
        ' QUALT1',' QUALT2',]
starred = 0
MXCNUM = len(numo) - 2
MXCTD = 8000
MAX_EXTRA_KEYWORDS = 10
MAX_LINES = 130
MAX_SUMMARY_LINES = 5000
MXINDEX = 400
MXERR = 12

QCODE_DQE     = 1
QCODE_PICKY   = 2
QCODE_ARCHIVE = 3
_qcode = {'-dqe'    : QCODE_DQE,
          '-picky'  : QCODE_PICKY,
          '-archive': QCODE_ARCHIVE}

USE_QUALT1 = 1
USE_QUALT2 = 2
qualt      = USE_QUALT1
_qualt_set = False

use_carter_tables = True
qcode_mode        = QCODE_ARCHIVE
_qcode_mode_set   = False

ocntry = None

_verbose = True
_die_on_too_many_errs = True

out_filename = None
wct_filename = None
sum_filename = []

HELP_MSG = '''    wctcvt-py alpha
usage: wctcvt [(-h|--help)]
              [(-qualt1|-qualt2)]
              [(-dqe|-picky|-archive)]
              [(-o|--output-file) OUPUT_FILE]
              [(-l|--list-file) LIST_FILE]
              [(-c|--country) CO]
              [(-q|--quiet)]
              FILE
Quality code modes:
DQE\t\tall data considered good
picky\t\tonly good data allowed
archive\t\tuncertain codes preserved'''
PRGM_HDR = '''\t*** WCTCVT ALPHA 2010-04-05 ***
 program wctcvt converts WOCE format CTD data
   to a more workable format.'''
QCODE_DESC = ''' quality code mode:
  1. DQE        all data considered good
  2. picky      good data only
  3. archive    uncertain codes preserved'''

##################################################
'''
Carter table atb loader
Loads the global variable atb for Carter table calculations
(cartertab.py). Requires carter_table_atb.hex; contents are
<6128> <numbers ...>
'''

CARTERTAB_ATB = 'carter_table_atb.hex'
CARTERTAB_TB  = 'carter_table_tb.hex'

atb = []
tb  = []

def load_carter_table_file(filename):
  a = []
  __carter_table_contents = ''
  with open(filename, 'rb') as file:
    for line in file:
      __carter_table_contents += line

  # discard number of entries
  # (which were included for backward compatibility)
  __carter_table_contents = __carter_table_contents[2:]

  for i in range(0, len(__carter_table_contents), 2):
    a.append( (ord(__carter_table_contents[i+1]) << 8) |
                ord(__carter_table_contents[i]) )
  return a
