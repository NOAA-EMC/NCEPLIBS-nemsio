cmake_minimum_required(VERSION 3.16)
project(nemsio VERSION 2.2.3)
set(${PROJECT_NAME}_VERSION ${PROJECT_VERSION} CACHE INTERNAL "${PROJECT_NAME} version number")
enable_language (Fortran)

add_custom_target(w3nco)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are: PRODUCTION Debug Release."
    FORCE)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  message("setting intel true")
  set(IntelComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU*" OR CMAKE_CXX_COMPILER_ID MATCHES "Clang*")
  set(GNUComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "pgc*")
  set(PGIComp true )
endif()

find_package(MPI REQUIRED)

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEBUG" BUILD_DEBUG)

if( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
  if(IntelComp)
    include_directories(${MPI_INCLUDE_PATH} )
    set(fortran_flags "-O2" "-convert big_endian" "-free" "-assume byterecl"
      "-fp-model strict" "-traceback"  "-g" "-mkl" "-qopenmp")
  elseif(GNUComp)
    set(fortran_flags "-O2" "-ffree-form")
  else()
    message("unknown compiler!")
    exit()
  endif()
endif()


file(GLOB fortran_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f90 )

set(lib_name ${PROJECT_NAME})
set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION})

add_library(${lib_name} STATIC ${fortran_src})
set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME "${versioned_lib_name}")

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include")
set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

target_include_directories(${lib_name} PRIVATE ${MPI_Fortran_INCLUDE_DIRS})

target_include_directories(${lib_name} PUBLIC
  $<BUILD_INTERFACE:${module_dir}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include>)

install(TARGETS ${lib_name}
  EXPORT ${PROJECT_NAME}-config
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib) 
install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
install(EXPORT ${PROJECT_NAME}-config DESTINATION ${CMAKE_INSTALL_PREFIX})
