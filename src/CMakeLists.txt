set(fortran_src
    nemsio_module.f90
    nemsio_module_mpi.f90
    nemsio_openclose.f90
    nemsio_read.f90
    nemsio_write.f90)

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
