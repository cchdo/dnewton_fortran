#!/usr/bin/env python
import optparse, sys

ophsc  = 'store_const'
ophsf  = 'store_false'

oph_Qn = 'Quality flags to use ([12])'
oph_q1 = 'Use QUALT1'
oph_q2 = 'Use QUALT2'
oph_Qd = 'qualt'
oph_qm = 'qmode'
oph_md = 'DQE: all data considered good'
oph_mp = 'Picky: good data only'
oph_ma = 'Archive: all qflags preserved'
oph_of = 'Specify output file'
oph_lf = 'Specify list file'
oph_oc = 'Specify originating country'
oph_sh = 'Suppress messaging'

_p = optparse.OptionParser()
_p.set_defaults(verbose=True,qualt=1,qmode=3)
_p.set_defaults(outfile=None,wctfile=None,ocntry=None)

_uq_grp = optparse.OptionGroup(_p,'Quality flags','')
_uq_grp.add_option('-Q',type='int',dest=oph_Qd,help=oph_Qn)
_uq_grp.add_option('--q1','--qualt1',action=ophsc,const=1,dest=oph_Qd,help=oph_q1)
_uq_grp.add_option('--q2','--qualt2',action=ophsc,const=2,dest=oph_Qd,help=oph_q2)

_qm_grp = optparse.OptionGroup(_p,'Quality code modes','')
_qm_grp.add_option('--dqe',    action=ophsc,const=1,dest=oph_qm,help=oph_md)
_qm_grp.add_option('--picky',  action=ophsc,const=2,dest=oph_qm,help=oph_mp)
_qm_grp.add_option('--archive',action=ophsc,const=3,dest=oph_qm,help=oph_ma)

_p.add_option('-o','--output-file',dest='outfile',help=oph_of)
_p.add_option('-l','--list-file',  dest='wctfile',help=oph_lf)
_p.add_option('-c','--country',dest='ocntry',help=oph_oc)
_p.add_option('-q','--quiet',action=ophsf,dest='verbose',help=oph_sh)

_p.add_option_group(_uq_grp)
_p.add_option_group(_qm_grp)

def main():
  global _p
  #try:
  options, args = _p.parse_args()
  if options.verbose:
    print 'verbose'
  else:
    print 'quiet'
  print 'using qualt%d\nusing qmode%d'%(options.qualt,options.qmode)
  if options.outfile is not None:
    print 'output file is %s'%options.outfile
  if options.wctfile is not None:
    print 'list   file is %s'%options.wctfile
  if options.ocntry is not None:
    print 'ocntry      is %s'%options.ocntry
  #except Error, err:
  #  print>>sys.stderr,str(err)

main()
