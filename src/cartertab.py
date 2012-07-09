#!/usr/bin/env python

execfile('libwocecvt.py')
execfile('libwctcvt.py')

def finar(latitude, longitud):
  global atb, _verbose

  if len(atb) == 0:
    load_carter_table_file(CARTERTAB_ATB)

  if abs(latitude) > 90.0 or abs(longitud) > 540.0:
    return -999

  lat = (int(latitude), int((latitude - int(latitude)) * 100.0))
  lon = (int(longitud), int((longitud - int(longitud)) * 100.0))

  if latitude < 0.0 and abs(lat[1]) > 50:
    lat[0] -= 1
  if longitud < 0.0 and abs(lon[1]) > 50:
    lon[0] -= 1
  lat[0] = 90 - lat[0]

  if lat[0] == 0:
    return 1

  base  = atb[lat[0]] + 1
  start = 0
  end   = (atb[lat[0] + 1] - base) / 2 - 1

  while True:
    if start > end:
      return -999
    ptr = (start + end) / 2
    lonc = atb[ptr * 2 + base]
    if lon[0] < lonc:
      end = ptr - 1
    elif lon[0] == lonc:
      if lon[0] < lonc:
        if ptr == 0:
          ptr = (atb[lat[0] + 1] - base) / 2 - 1
        else:
          ptr -= 1
      return atb[ptr * 2 + base + 1]
    else:
      start = ptr + 1
  if _verbose:
    print>>sys.stderr,'cartertab: something went very wrong in finar();',
    print>>sys.stderr,'please contact cchdo@ucsd.edu'
  return -999 # just in case

def dcorr(depth, area):
  global tb, _verbose
  if len(tb) == 0:
    load_carter_table_file(CARTERTAB_TB)
  if area < 0 or area > 85:
    return -999
  if depth < 200:
    return depth
  start = tb[area] + 1
  end   = tb[area + 1] - 1
  ind   = (end - start) * 100 + 200
  if depth > ind:
    ind -= 100
    start = end - 1
  else:
    start += (depth - 200) / 100
    end    = start + 1
    ind    = (depth / 100) * 100

  return ((tb[end] - tb[start]) * (depth - ind) + 50) / 100 + tb[start]
