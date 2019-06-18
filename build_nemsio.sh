#!/bin/sh

 : ${THISDIR:=$(dirname $(readlink -f -n ${BASH_SOURCE[0]}))}
 CDIR=$PWD; cd $THISDIR

 source ./Conf/Analyse_args.sh
 source ./Conf/Collect_info.sh
 source ./Conf/Gen_cfunction.sh
 source ./Conf/Reset_version.sh

 if [[ ${sys} == "intel_general" ]]; then
   sys6=${sys:6}
   source ./Conf/Nemsio_${sys:0:5}_${sys6^}.sh
 elif [[ ${sys} == "gnu_general" ]]; then
   sys4=${sys:4}
   source ./Conf/Nemsio_${sys:0:3}_${sys4^}.sh
 else
   source ./Conf/Nemsio_intel_${sys^}.sh
 fi
 $CC --version &> /dev/null || {
   echo "??? NEMSIO: compilers not set." >&2
   exit 1
 }
 [[ -z $NEMSIO_VER || -z $NEMSIO_LIB ]] && {
   echo "??? NEMSIO: module/environment not set." >&2
   exit 1
 }

set -x
 nemsioLib=$(basename ${NEMSIO_LIB})
 nemsioInc=$(basename ${NEMSIO_INC})

#################
 cd src
#################

 $skip || {
#-------------------------------------------------------------------
# Start building libraries
#
 echo
 echo "   ... build nemsio library ..."
 echo
   make clean LIB=$nemsioLib MOD=$nemsioInc
   mkdir -p $nemsioInc
   MPIFFLAGS="$MPIFFLAGS ${MODPATH}$nemsioInc"
   collect_info nemsio - OneLine LibInfo
   nemsioInfo=nemsio_info_and_log.txt
   $debg && make debug LIB=$nemsioLib &> $nemsioInfo \
         || make build LIB=$nemsioLib &> $nemsioInfo
   make message MSGSRC="$(gen_cfunction $nemsioInfo OneLine LibInfo)" \
                LIB=$nemsioLib
 }

 $inst && {
#
#     Install libraries and source files 
#
   $local && {
     instloc=..
     LIB_DIR=$instloc
     INCP_DIR=$instloc/include
     [ -d $INCP_DIR ] || { mkdir -p $INCP_DIR; }
     SRC_DIR=
   } || {
     [[ $instloc == --- ]] && {
       LIB_DIR=$(dirname $NEMSIO_LIB)
       INCP_DIR=$(dirname $NEMSIO_INC)
       SRC_DIR=$NEMSIO_SRC
     } || {
       LIB_DIR=$instloc
       INCP_DIR=$instloc/include
       SRC_DIR=$instloc/src
       [[ $instloc == .. ]] && SRC_DIR=
     }
     [ -d $LIB_DIR ] || mkdir -p $LIB_DIR
     [ -d $NEMSIO_INC ] && { rm -rf $NEMSIO_INC; } \
                        || { mkdir -p $INCP_DIR; }
     [ -z $SRC_DIR ] || { [ -d $SRC_DIR ] || mkdir -p $SRC_DIR; }
   }

   make clean LIB=
   make install LIB=$nemsioLib MOD=$nemsioInc \
                LIB_DIR=$LIB_DIR INC_DIR=$INCP_DIR SRC_DIR=$SRC_DIR
 }

