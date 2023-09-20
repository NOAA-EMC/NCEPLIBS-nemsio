![Status](https://github.com/NOAA-EMC/NCEPLIBS-nemsio/workflows/Build%20and%20Test/badge.svg)

# NEMSIO

The NCEPLIBS-nemsio library and utilities perform I/O for the NCEP
models using NOAA Environmental Modeling System (NEMS) format. The
NCEPLIBS-nemsio code is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For complete documentation see
https://noaa-emc.github.io/NCEPLIBS-nemsio/.

For more information about NEMS, see the [NEMS NMMB documentation
site](https://nomads.ncep.noaa.gov/txt_descriptions/NEMS_NMM_doc.shtml).

To submit bug reports, feature requests, or other code-related issues including installation and usage questions, please create a [GitHub issue](https://github.com/NOAA-EMC/NCEPLIBS-nemsio/issues). For general NCEPLIBS inquiries, contact [Edward Hartnett](mailto:edward.hartnett@noaa.gov) (secondary point of contact [Alex Richert](mailto:alexander.richert@noaa.gov)).

## Authors

M.Iredell, J.Wang, Hang Lei, mahajan, other NCEP/EMC developers.

Code manager: [Hang Lei](mailto:hang.lei@noaa.gov)

## Prerequisites

This library requires the
[NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio) and the
[NCEPLIBS-w3emc](https://github.com/NOAA-EMC/NCEPLIBS-w3emc)
libraries.

This library also requires an MPI-enabled Fortran compiler.

## Installing

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install -DCMAKE_PREFIX_PATH="/path/to/bacio;/path/to/w3emc" ..
make -j2
make install
```

### Utilities
- `nemsio_get` - get the value of a variable in the file
- `nemsio_read` - read a nemsio file and print statistics to screen
- `nemsio_chgdate` - change datetime stamp in the nemsio file
- `mkgfsnemsioctl` - create GrADS ctl file to read nemsio data in GrADS

### Testing
`ctest` is used to read a test file.

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.

