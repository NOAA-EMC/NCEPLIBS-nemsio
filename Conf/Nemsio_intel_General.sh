# *** manually set environments (for intel compiler) of nemsio ***

# !!! module environment (*THEIA*) !!!
 module load intel/18.1.163
 module load impi/2018.0.1

 ANCHORDIR=..
 export COMP=ips/impi
 export NEMSIO_VER=v2.2.4
 export NEMSIO_SRC=
 export NEMSIO_INC=$ANCHORDIR/${COMP#*/}/include/nemsio_${NEMSIO_VER}
 export NEMSIO_LIB=$ANCHORDIR/${COMP#*/}/libnemsio_${NEMSIO_VER}.a

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export FFLAGS="-O3 -xHOST -traceback -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-O3 -xHOST -traceback -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS=""

 export USECC=""
 export USEFC="YES"
 export DEPS=""
