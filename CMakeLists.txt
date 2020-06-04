cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
  nemsio
  VERSION ${pVersion}
  LANGUAGES Fortran)

enable_testing()

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -traceback -convert big_endian -free -assume byterecl -fp-model strict -mkl"
  )
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -fconvert=big-endian -ffree-form")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
endif()

find_package(MPI REQUIRED COMPONENTS Fortran)

if(NOT TARGET bacio_4)
  find_package(bacio REQUIRED)
endif()

if(NOT TARGET w3nco_d)
  find_package(w3nco REQUIRED)
endif()

add_subdirectory(src)
add_subdirectory(utils)
add_subdirectory(unit_test)
