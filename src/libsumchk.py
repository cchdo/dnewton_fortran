#!/usr/bin/env python

execfile('libwocecvt.py')

# SUMlim constants
SUMLIM_ERR = 32

# accepted column headings and search delimiter indices for each
HEADINGS = { 0:('EXPOCODE',4), 1:('SECTION ',4), 2:('STNNBR  ',4), 3:('CASTNO  ',4),
             4:('TYPE    ',4), 5:('DATE    ',4), 6:('TIME    ',4), 7:(' CODE   ',5),
             8:('LATITUDE',3), 9:('LONGITUD',3),10:('NAV_CODE',3),11:('DEPTH_UN',5),
            12:('BOTTOMHT',5),13:('WHEEL_MT',4),14:('PRESS_MX',4),15:('BOTTLES ',5),
            16:('PARAMS  ',4),17:('COMMENTS',3),18:('CDEPTH  ',4),}

# SUMqwk constants
#      ID: (max,name)
MAX = { 0: (14, 'expocode'          ),
        1: (6 , 'whpid (section)'   ),
        2: (6 , 'stnnbr'            ),
        3: (3 , 'castno'            ),
        4: (3 , 'cast type'         ),
        5: (10, 'date'              ),  # NB. no limit (YYYY-MM-DD)
        6: (5 , 'time'              ),  # NB. no limit (HH:MM)
        7: (2 , 'time code'         ),
        8: (12, 'latitude'          ),  # NB. no limit (DD MM.MMM H)
        9: (12, 'longitude'         ),  # NB. no limit (DD MM.MMM H)
       10: (3 , 'nav code'          ),
       11: (8 , 'bottom depth (unc)'),  # NB. no limit (dddd.ddd)
       12: (8 , 'height over bottom'),  # NB. no limit (hhhh.hhh)
       13: (5 , 'meter wheel'       ),  # NB. no limit [?]
       14: (5 , 'max pressure'      ),  # NB. no limit (p.ppp)
       15: (2 , 'number of bottles' ),  # NB. no limit (nn)
       16: (20, 'params'            ),
       17: (30, 'comment'           ),
       18: (8 , 'bottom depth (cor)'),} # NB. no limit (dddd.ddd)

''' MAXIMUM(param,)
Gets the accepted maximum number of columns a parameter can span.
param          parameter ID (see sumlim consts)
'''
def MAXIMUM(param,):
  return MAX[param][0]

''' NAME(param,)
Gets the name of a parameter.
param          parameter ID (see sumlim consts)
'''
def NAME(param,):
  return MAX[param][1]

''' MISSING(param, utility,)
Generates an error message for a missing parameter.
param          parameter ID (see sumlim consts)
utility        name of utility this message is being generated for
'''
def MISSING(param, utility,):
  return " %s: missing %s (%d)" % (utility, NAME(param), param)

''' OVERLARGE(param, utility,)
Generates an error message for a parameter that exceeds its allotted
column span.
param          parameter ID (see sumlim consts)
utility        name of utility this message is being generated for
'''
def OVERLARGE(param, utility,):
  if param not in ['params', 'comment']:
    # ignore PARAMS and COMMENTS (see below)
    return " %s: %s is too big. max=%d"%(utility,
                        NAME(param),MAXIMUM(param))
  else: # truncate PARAMS and COMMENTS
    return " %s: parameter list truncated." % utility

# SUMqwk variables
GLOBAL_EXPOCODE = None

# SUMun1 constants
HASH_NAMES = ['EXPO','WHPID','STNNBR','CASTNO','CTYPE','DATE',
     'TIME','TCODE','LATITUDE','LONGITUD','NAV','BOTMUN','BOTMCO',
     'HAB','WHEEL','MXPRES','NBOTS','PARAMS','COMMENTS','SHIP',]

# sumchk main variables
MAX_ERROR               = 20    # max errors before abort
_max_error_set          = False # whether MAX_ERROR has been set
MAX_PROCESSABLE_LINES   = 1000  # (deprecated) max lines that checker can do
_die_on_too_many_errors = True  # whether to abort on too many errors
_perr                   = True

# error reporting arrays
read_err  = []
overlarge = []
misplaced = []
missing   = []
unknown   = []
strange   = []
current_line = 0
_illegal_no_data_dash = False
