@mainpage

# Introduction

The NCEPLIBS-nemsio library and utilities perform I/O for the NCEP models using NOAA Environmental Modeling System (NEMS) format.

## Installation

```
git clone https://github.com/NOAA-EMC/NCEPLIBS-nemsio # or download a release from https://github.com/NOAA-EMC/NCEPLIBS-nemsio/releases
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install -S NCEPLIBS-nemsio -B NCEPLIBS-nemsio/build # <add'l CMake options>
cmake --build NCEPLIBS-nemsio/build --parallel 4
ctest --test-dir NCEPLIBS-nemsio/build --parallel 4 # <add'l CTest options>
cmake --install NCEPLIBS-nemsio/build
```

The following CMake build options can be used to configure the build by setting them with `-D<OPTION>=<VALUE>`.

| Option | Description | Default |
|--------|-------------|---------|
| CMAKE_INSTALL_PREFIX | Installation path | /usr/local |
| CMAKE_POSITION_INDEPENDENT_CODE | Enable position-independent code (PIC) for static build | OFF |
| ENABLE_DOCS | Enable generation of doxygen-based documentation. | OFF |
| ENABLE_MPI | Enable MPI I/O with nemsio_module_mpi | ON |
