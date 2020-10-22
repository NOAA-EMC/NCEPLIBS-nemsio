# This is the main CMake file for NCEPLIBS-nemsio.
#
# Mark Potts, Kyle Gerheiser
cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
  nemsio
  VERSION ${pVersion}
  LANGUAGES Fortran)

include(GNUInstallDirs)

option(ENABLE_TESTS "Build pfunit tests?" OFF)
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

find_package(MPI REQUIRED COMPONENTS Fortran)

find_package(bacio REQUIRED)
find_package(w3nco REQUIRED)

add_subdirectory(src)
add_subdirectory(utils)

if(ENABLE_TESTS)
  find_package(PFUNIT REQUIRED)
  enable_testing()
  add_subdirectory(tests)
endif()

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)  

