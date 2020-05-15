set(tstName read_nemsio)
set(srcName read_nemsio.f90)
set(exeName read_nemsio.x)

execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink
                ${CMAKE_CURRENT_SOURCE_DIR}/testdataset
                ${CMAKE_CURRENT_BINARY_DIR}/testdataset)

add_executable(${exeName} ${srcName})
add_dependencies(${exeName} nemsio bacio_4 w3nco_d)
target_link_libraries( ${exeName} PUBLIC nemsio bacio_4 w3nco_d)
add_test(NAME ${tstName} COMMAND ${exeName} testdataset)
