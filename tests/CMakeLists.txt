# This is the CMake file for the tests directory of the
# NCEPLIBS-nemsio project.
#
# Kyle Gerheiser

set(tstName read_nemsio)
set(srcName read_nemsio.f90)
set(exeName read_nemsio.x)

execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink
                ${CMAKE_CURRENT_SOURCE_DIR}/testdataset
                ${CMAKE_CURRENT_BINARY_DIR}/testdataset)

add_executable(${exeName} ${srcName})
add_dependencies(${exeName} nemsio)
target_link_libraries( ${exeName} PUBLIC nemsio bacio::bacio_4 w3emc::w3emc_d)
add_test(NAME ${tstName} COMMAND ${exeName} testdataset)



