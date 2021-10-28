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
include(CTest)

option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
option(ENABLE_MPI "Enable MPI I/O with nemsio_module_mpi" ON)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(ENABLE_MPI)
  find_package(MPI REQUIRED COMPONENTS Fortran)
endif()

if (MPI_Fortran_FOUND)
  message(STATUS "Parallel NEMSIO Enabled")
else()
  message(STATUS "Parallel NEMSIO Disabled")
endif()

find_package(bacio 2.3.0 REQUIRED)
find_package(w3emc 2.9.0 REQUIRED)

if(bacio_VERSION GREATER_EQUAL 2.5.0)
  set(bacio_name bacio)
else()
  set(bacio_name bacio_4)
endif()

add_subdirectory(src)
add_subdirectory(utils)

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)

