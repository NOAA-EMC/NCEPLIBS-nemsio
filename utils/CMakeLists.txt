add_executable(nemsio_read nemsio_read.f90)
target_link_libraries(nemsio_read nemsio bacio::bacio_4 w3nco::w3nco_d)

add_executable(nemsio_get nemsio_get.f90)
target_link_libraries(nemsio_get nemsio bacio::bacio_4 w3nco::w3nco_d)

add_executable(nemsio_chgdate nemsio_chgdate.f90)
target_link_libraries(nemsio_chgdate nemsio bacio::bacio_4 w3nco::w3nco_d)

add_executable(mkgfsnemsioctl mkgfsnemsioctl.f90)
target_link_libraries(mkgfsnemsioctl nemsio bacio::bacio_4 w3nco::w3nco_d)

install(TARGETS nemsio_read    RUNTIME DESTINATION bin)
install(TARGETS nemsio_get     RUNTIME DESTINATION bin)
install(TARGETS nemsio_chgdate RUNTIME DESTINATION bin)
install(TARGETS mkgfsnemsioctl RUNTIME DESTINATION bin)
