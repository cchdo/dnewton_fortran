#!/usr/bin/env python

# Global constants
MXHDNG = 19

# Diagnostic helpers
BOLD = "\x1b\x5b\x31;37;40m"
CLEAR = "\x1b\x5b\x30m"

# Parameter index constants
EXPOCODE  = 0
SECTION   = 1
STNNBR    = 2
CASTNO    = 3
CTYPE     = 4
DATE      = 5
TIME      = 6
TCODE     = 7
LATITUDE  = 8
LONGITUD  = 9
NAV_CODE  = 10
DEPTH_UNC = 11
BOTTOMHT  = 12
WHEEL_MT  = 13
PRESS_MX  = 14
BOTTLES   = 15
PARAMS    = 16
COMMENTS  = 17
CDEPTH    = 18

# Auxiliary constant names
CASTTYPE  = CTYPE
TIMECODE  = TCODE

# Known (accepted) timecodes
KNOWN_TCODES = ['AT','BE','BO','DE','EN','MR','RE','UN',]
