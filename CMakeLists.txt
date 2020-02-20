cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
  nemsio
  VERSION ${pVersion}
  LANGUAGES Fortran)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang|PGI)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -convert big_endian -free -assume byterecl -fp-model strict -mkl"
  )
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -fconvert=big-endian -ffree-form")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
endif()

find_package(MPI REQUIRED COMPONENTS Fortran)

set(fortran_src
    src/nemsio_module.f90 src/nemsio_module_mpi.f90 src/nemsio_openclose.f90
    src/nemsio_read.f90 src/nemsio_write.f90)

set(lib_name ${PROJECT_NAME})
set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION})
set(module_dir ${CMAKE_CURRENT_BINARY_DIR}/include)

add_library(${lib_name} STATIC ${fortran_src})
set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME ${versioned_lib_name})
set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY
                                             ${module_dir})
target_include_directories(${lib_name} PRIVATE ${MPI_Fortran_INCLUDE_DIRS})
target_include_directories(
  ${lib_name} PUBLIC $<BUILD_INTERFACE:${module_dir}>
                     $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include>)

install(
  TARGETS ${lib_name}
  EXPORT ${PROJECT_NAME}-config
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)
install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})

install(EXPORT ${PROJECT_NAME}-config
        DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/cmake/${PROJECT_NAME})
