#!/bin/sh --login

#-----------------------------------------------------
# Build the nemsio library 
#
# $> build.sh machine_name
#-----------------------------------------------------

#set -x

target=$1
if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

module purge
if [ $target = wcoss ]; then
 module load ics/14.0.1
 export FC='mpiifort'
elif [ $target = cray ]; then
 module load PrgEnv-intel
 module load craype-sandybridge
 export FC='ftn'
elif [$target = theia ]; then
 module load intel
 export FC='mpiifort'
fi
module list

export FFLAGS='-O -g'
export AR='ar'
export ARFLAGS='-rvu'
export RM='rm'

echo FC=$FC

make clean
make

exit


