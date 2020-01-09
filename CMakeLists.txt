cmake_minimum_required(VERSION 2.6)
project(nemsio)
set(VERSION v2.2.3)
enable_language (Fortran)
if (NOT CMAKE_BUILD_TYPE)
    set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
        "Choose the type of build, options are: PRODUCTION Debug Release."
        FORCE)
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  message("setting intel true")
  set(IntelComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU*")
  set(GNUComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "pgc*")
  set(PGIComp true )
endif()
find_package(MPI REQUIRED)

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEGUG" BUILD_DEBUG)

set(CMAKE_Fortran_COMPILER ${MPI_Fortran_COMPILER})

if( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
  if(IntelComp)
    message("setting intel flags")
    include_directories(${MPI_INCLUDE_PATH} )
#   set(CMAKE_Fortran_FLAGS "-O2 -convert big_endian -free -assume byterecl -xHOST -fp-model strict -traceback -axCORE-AVX2 " CACHE INTERNAL "" )
    set(CMAKE_Fortran_FLAGS " -O2 -convert big_endian -free -assume byterecl -fp-model strict -traceback  -g -mkl -qopenmp " CACHE INTERNAL "" )
#   set(CMAKE_Fortran_FLAGS "-g -traceback -O3 -fPIC" CACHE INTERNAL "" )
  elseif(GNUComp)
    set(CMAKE_Fortran_FLAGS "-O2 -ffree-form " CACHE INTERNAL "" )
    include_directories(${MPI_INCLUDE_PATH}/gfortran/8.2.0 )
  elseif(PGIComp)
  else()
    message("unkown compiler!")
    exit()
  endif()
endif()

set(LIBNAME ${CMAKE_PROJECT_NAME}_${VERSION})
file(GLOB LIB_SRC ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f90 )
add_library(${LIBNAME} STATIC ${LIB_SRC} )
set(CMAKE_INCLUDE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include")
set_target_properties(${LIBNAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY})
target_include_directories(${LIBNAME} PUBLIC ${CMAKE_INCLUDE_OUTPUT_DIRECTORY})

file(MAKE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY})

install(TARGETS ${LIBNAME}
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

install(DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/ DESTINATION include )
