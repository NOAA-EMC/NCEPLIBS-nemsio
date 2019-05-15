#!/bin/sh

 (( $# == 0 )) && {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [[local]install[only]]"
   exit 1
 }

 sys=${1,,}
 [[ $sys == wcoss || $sys == dell || $sys == cray ||\
    $sys == theia || $sys == intel_general || $sys == gnu_general ]] || {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [[local]install[only]]"
   exit 1
 }
 debg=false
 inst=false
 skip=false
 local=false
 (( $# > 1 )) && {
   [[ ${2,,} == build ]] && debg=false
   [[ ${2,,} == debug ]] && debg=true
   [[ ${2,,} == install ]] && inst=true
   [[ ${2,,} == localinstall ]] && { local=true; inst=true; }
   [[ ${2,,} == installonly ]] && { inst=true; skip=true; }
   [[ ${2,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
 }
 (( $# > 2 )) && {
   [[ ${3,,} == build ]] && debg=false
   [[ ${3,,} == debug ]] && debg=true
   [[ ${3,,} == install ]] && inst=true
   [[ ${3,,} == localinstall ]] && { local=true; inst=true; }
   [[ ${3,,} == installonly ]] && { inst=true; skip=true; }
   [[ ${3,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
 }

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
 [[ -z $NEMSIO_VER || -z $NEMSIO_LIB ]] && {
   echo "??? NEMSIO: module/environment not set."
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
              LIB_DIR=..
              INCP_DIR=..
              SRC_DIR=
             } || {
              LIB_DIR=$(dirname $NEMSIO_LIB)
              INCP_DIR=$(dirname $NEMSIO_INC)
              SRC_DIR=$NEMSIO_SRC
              [ -d $LIB_DIR ] || mkdir -p $LIB_DIR
              [ -d $NEMSIO_INC ] && { rm -rf $NEMSIO_INC; } \
                                 || { mkdir -p $INCP_DIR; }
              [ -z $SRC_DIR ] || { [ -d $SRC_DIR ] || mkdir -p $SRC_DIR; }
             }


   make clean LIB=
   make install LIB=$nemsioLib MOD=$nemsioInc \
                LIB_DIR=$LIB_DIR INC_DIR=$INCP_DIR SRC_DIR=$SRC_DIR
 }

