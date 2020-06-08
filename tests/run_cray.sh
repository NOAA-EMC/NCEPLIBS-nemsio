#!/bin/ksh

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -oo log.fcst
#BSUB -eo log.fcst
#BSUB -J chgres_fcst2
#BSUB -q debug
#BSUB -M 2000
#BSUB -W 00:06
#BSUB -extsched 'CRAYLINUX[]'

set -ax

 . $MODULESHOME/init/sh
module load PrgEnv-intel

export NODES=1

export KMP_AFFINITY=disabled
export OMP_NUM_THREADS_CH=24
export APRUNC="aprun -n 1 -N 1 -j 1 -d 24 -cc depth"

./nemsio_read testdataset >lei.log
#$CHGRESSH
rm -f chgres.*
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "***ERROR*** rc= $rc"
  exit
fi
