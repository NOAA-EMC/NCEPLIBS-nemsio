# NEMSIO

Performs I/O for the NCEP models using NEMS.

Code manager: Hang Lei


### Prerequisites

Compilers: GNU | Intel | Clang | AppleClang
with MPI


### Installing

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install /path/to/NCEPLIBS-nemsio
make -j2
make install
```

### Utilities
- `nemsio_get` - get the value of a variable in the file
- `nemsio_read` - read a nemsio file and print statistics to screen
- `nemsio_chgdate` - change datetime stamp in the nemsio file
- `mkgfsnemsioctl` - create GrADS ctl file to read nemsio data in GrADS

### Testing
`ctest` is used to read a test file. More tests will be added as needed


### Version

2.5.0


### Authors

* **[NCEP/EMC](mailto:NCEP.List.EMC.nceplibs.Developers@noaa.gov)**
