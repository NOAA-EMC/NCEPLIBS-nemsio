!----------------------------------------------------------------------------
module nemsio_read
!
!$$$ documentation clock
!
! module: nemsio_read      read data fields from a nemsio file
!  Programmer: J. Wang          date: 2011-01-13
!
! abstract: this module provides subroutines to read data fields out of a
!           nemsio file. The data file could be 'bin4','bin8', or 'grib',
!           The dat field could be read out by the record number
!           or by given the data field name, level type and level. Overload 
!           interfaces are provided to handle different data type (real(4)
!           or real(8) ) of the array that holds the data.
!
! Possible return code
!          0   Successful call
!         -31  get dimension ffrom gfile
!         -32  read data field by record number using w3d
!         -33  read data field by given data field name,level type and level using w3d
!         -34  read data field by record number using w34
!         -35  read data field by given data field name,level type and level using w34
!         -41  read data field by record number from 4 byte real binary file 
!         -42  read data field by record number from 8 byte real binary file 
!         -43  read data field by field name,levtyp and lev from 4 byte real binary file 
!         -44  read data field by field name,levtyp and lev from 8 byte real binary file 
!         -45  read data field by record number using w34 from grib data
!         -46  read data field by field name,level type and level using w34 from grib data
!         -47  read data field by record number using w3d from grib data
!         -48  read data field by field name,level type and level using w3d from grib data
!------------------------------------------------------------------------------
!
  use nemsio_openclose
!
  implicit none
!
  private
!------------------------------------------------------------------------------
!----- interface
!
  interface nemsio_readrec
    module procedure nemsio_readrec4
    module procedure nemsio_readrec8
  end interface nemsio_readrec
!
  interface nemsio_readrecv
    module procedure nemsio_readrecv4
    module procedure nemsio_readrecv8
  end interface nemsio_readrecv
!
  interface nemsio_readrecw34
    module procedure nemsio_readrec4w34
    module procedure nemsio_readrec8w34
  end interface nemsio_readrecw34
!
  interface nemsio_readrecvw34
    module procedure nemsio_readrecv4w34
    module procedure nemsio_readrecv8w34
  end interface nemsio_readrecvw34
!
!public mehtods
  public nemsio_readrec,nemsio_readrecv,nemsio_readrecw34,nemsio_readrecvw34
!
!---------------------------------------------------------
!
contains
!
!------------------------------------------------------------------------------
!
  subroutine nemsio_getgfile(gfile,read_ldata,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    type(nemsio_read_localdata),intent(out)       :: read_ldata
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer ios
!
    if(present(iret)) iret= -31
!
    call nemsio_getfilehead(gfile,iret=ios,read_ldata=read_ldata)
    if(ios/=0) then
       if(present(iret)) then
         iret=ios
         return
       else
         print *,'ERROR: NEMSIO readrec in getting file head'
         stop
       endif
    endif
!
  end subroutine nemsio_getgfile
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrec4(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)  :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable            :: datatmp8(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-32
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!---
   if ( read_ldata%mygdatatype .eq. 'bin4') then 
     if(.not.present(nframe) ) then
       call nemsio_readrecbin4d4(gfile,jrec,data,ios)
     else
      allocate(datatmp(read_ldata%myfieldsize) )
      call nemsio_readrecbin4d4(gfile,jrec,datatmp,ios)
     endif
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
     allocate(datatmp8(read_ldata%myfieldsize) )
     call nemsio_readrecbin8d8(gfile,jrec,datatmp8,ios)
   else
     allocate(datatmp8(read_ldata%myfieldsize) )
     call nemsio_readrecgrb8(gfile,jrec,datatmp8,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(read_ldata%mygdatatype .eq. 'bin4') then
       do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
        do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
         data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe        &
           +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
        enddo
       enddo
       deallocate(datatmp)
     elseif(read_ldata%mygdatatype=='bin8'.or.read_ldata%mygdatatype=='grib') then
       do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
        do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
         data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp8(i+nframe        &
           +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
        enddo
       enddo
       deallocate(datatmp8)
     endif
   else
     if(read_ldata%mygdatatype=='bin8'.or.read_ldata%mygdatatype=='grib') then
       data=datatmp8
       deallocate(datatmp8)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec4
!------------------------------------------------------------------------------
  subroutine nemsio_readrec8(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-32
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!
   if ( read_ldata%mygdatatype .eq. 'bin4') then
     allocate(datatmp4(read_ldata%myfieldsize))
     call nemsio_readrecbin4d4(gfile,jrec,datatmp4,ios)
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
     if(.not.present(nframe)) then
       call nemsio_readrecbin8d8(gfile,jrec,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize))
       call nemsio_readrecbin8d8(gfile,jrec,datatmp,ios)
     endif
   else
     if(.not.present(nframe)) then
       call nemsio_readrecgrb8(gfile,jrec,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize))
       call nemsio_readrecgrb8(gfile,jrec,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(read_ldata%mygdatatype=='bin4') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp4(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp4)
     elseif(read_ldata%mygdatatype=='bin8'.or.read_ldata%mygdatatype=='grib') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp)
     endif
   else
     if(read_ldata%mygdatatype=='bin4') then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv4(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable             :: datatmp8(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-33
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!---
   if ( read_ldata%mygdatatype .eq. 'bin4') then
     if(.not.present(nframe) ) then
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
     allocate(datatmp8(read_ldata%myfieldsize) )
     call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp8,ios)
   else
     allocate(datatmp8(read_ldata%myfieldsize) )
     call nemsio_readrecvgrb8(gfile,name,levtyp,lev,datatmp8,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(read_ldata%mygdatatype=='bin4') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp)
     elseif(read_ldata%mygdatatype=='bin8'.or.read_ldata%mygdatatype=='grib' ) then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp8(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp8)
     endif
   else
     if(read_ldata%mygdatatype=='bin8'.or.read_ldata%mygdatatype=='grib' ) then
       data=datatmp8
       deallocate(datatmp8)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv8(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 8 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-33
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!---
   if ( read_ldata%mygdatatype .eq. 'bin4') then
     allocate(datatmp4(read_ldata%myfieldsize) )
     call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp4,ios)
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
     if(.not.present(nframe) ) then
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else
     if(.not.present(nframe) ) then
       call nemsio_readrecvgrb8(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecvgrb8(gfile,name,levtyp,lev,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(read_ldata%mygdatatype=='bin4') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp4(i+nframe        &
           +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp4)
      elseif(read_ldata%mygdatatype=='bin8'.or.read_ldata%mygdatatype=='grib') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe        &
           +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp)
      endif
   else
     if(read_ldata%mygdatatype=='bin4') then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv8
!------------------------------------------------------------------------------
  subroutine nemsio_readrec4w34(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable             :: datatmp8(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-34
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!---
   if ( read_ldata%mygdatatype .eq. 'bin4') then
     if(.not.present(nframe)) then
       call nemsio_readrecbin4d4(gfile,jrec,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecbin4d4(gfile,jrec,datatmp,ios)
     endif
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
      allocate(datatmp8(read_ldata%myfieldsize) )
      call nemsio_readrecbin8d8(gfile,jrec,datatmp8,ios)
   else
     if(.not.present(nframe)) then
       call nemsio_readrecgrb4w34(gfile,jrec,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecgrb4w34(gfile,jrec,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(read_ldata%mygdatatype=='bin4'.or.read_ldata%mygdatatype=='grib') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp)
      elseif(read_ldata%mygdatatype=='bin8') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
          data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp8(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp8)
      endif
   else
     if(read_ldata%mygdatatype=='bin8') then
       data=datatmp8
       deallocate(datatmp8)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec4w34
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
  subroutine nemsio_readrec8w34(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-34
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!---
   if ( read_ldata%mygdatatype .eq. 'bin4') then
     allocate(datatmp4(read_ldata%myfieldsize) )
     call nemsio_readrecbin4d4(gfile,jrec,datatmp4,ios)
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
     if(.not.present(nframe) ) then
      call nemsio_readrecbin8d8(gfile,jrec,data,ios)
     else
      allocate(datatmp(read_ldata%myfieldsize) )
      call nemsio_readrecbin8d8(gfile,jrec,datatmp,ios)
     endif
   else
     allocate(datatmp4(read_ldata%myfieldsize) )
     call nemsio_readrecgrb4w34(gfile,jrec,datatmp4,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(read_ldata%mygdatatype .eq. 'bin4'.or.read_ldata%mygdatatype .eq. 'grib' ) then
       do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
       do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
        data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp4(i+nframe        &
          +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
       enddo
       enddo
       deallocate(datatmp4)
     else if(read_ldata%mygdatatype .eq. 'bin8') then
       do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
       do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
        data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe       &
          +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
       enddo
       enddo
       deallocate(datatmp)
     endif
   else
     if(read_ldata%mygdatatype .eq. 'bin4'.or.read_ldata%mygdatatype .eq. 'grib' ) then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec8w34
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv4w34(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable             :: datatmp8(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-35
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!---
   if ( read_ldata%mygdatatype .eq. 'bin4') then
     if(.not.present(nframe)) then
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
     allocate(datatmp8(read_ldata%myfieldsize) )
     call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp8,ios)
   else
     if(.not.present(nframe)) then
       call nemsio_readrecvgrb4w34(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecvgrb4w34(gfile,name,levtyp,lev,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(read_ldata%mygdatatype=='bin4'.or.read_ldata%mygdatatype=='grib') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
           data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp)
      elseif(read_ldata%mygdatatype=='grib8') then
        do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
         do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
           data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp8(i+nframe        &
            +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
         enddo
        enddo
        deallocate(datatmp8)
      endif
   else
      if(read_ldata%mygdatatype=='grib8') then
        data=datatmp8
        deallocate(datatmp8)
      endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv4w34
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv8w34(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    integer :: i,j,ios
    type(nemsio_read_localdata) :: read_ldata
!------------------------------------------------------------
! read 8 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-35
!
   call nemsio_getgfile(gfile,read_ldata,iret)
!---
   if ( read_ldata%mygdatatype .eq. 'bin4') then
     allocate(datatmp4(read_ldata%myfieldsize) )
     call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp4,ios)
   else if ( read_ldata%mygdatatype .eq. 'bin8') then
     if(.not.present(nframe)) then
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(read_ldata%myfieldsize) )
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else
     allocate(datatmp4(read_ldata%myfieldsize) )
     call nemsio_readrecvgrb4w34(gfile,name,levtyp,lev,datatmp4,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(read_ldata%mygdatatype .eq. 'bin4'.or.read_ldata%mygdatatype .eq. 'grib') then
       do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
       do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
        data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp4(i+nframe        &
          +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
       enddo
       enddo
       deallocate(datatmp4)
      elseif(read_ldata%mygdatatype .eq. 'bin8') then
       do j=1,read_ldata%mydimy+2*read_ldata%mynframe-2*nframe
       do i=1,read_ldata%mydimx+2*read_ldata%mynframe -2*nframe
        data(i+(j-1)*(read_ldata%mydimx+2*read_ldata%mynframe-2*nframe))=datatmp(i+nframe        &
          +(j-1+nframe)*(read_ldata%mydimx+2*read_ldata%mynframe))
       enddo
       enddo
       deallocate(datatmp)
     endif
   else
     if(read_ldata%mygdatatype .eq. 'bin4'.or.read_ldata%mygdatatype .eq. 'grib') then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv8w34
!------------------------------------------------------------------------------
!
!*****************   read bin data set :  ********************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin4d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    type(nemsio_read_localdata) :: read_ldata
!
   call nemsio_getgfile(gfile,read_ldata,iret)

    if(present(iret)) iret=-41
    iskip=read_ldata%mytlmeta+int(jrec-1,8)*int(kind(data)*read_ldata%myfieldsize+8,8)
    iread=int(nemsio_realkind,8)*int(size(data),8)
    call bafrreadl(read_ldata%myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(read_ldata%do_byteswap) call byteswap(data,nemsio_realkind,size(data))
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr
    type(nemsio_read_localdata) :: read_ldata

    if(present(iret)) iret=-42
!
   call nemsio_getgfile(gfile,read_ldata,iret)
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0)  return
    iskip=read_ldata%mytlmeta+int(jrec-1,8)*int(nemsio_realkind*read_ldata%myfieldsize+8,8)
    iread=int(kind(data),8)*int(size(data),8)
    call bafrreadl(read_ldata%myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(read_ldata%do_byteswap) call byteswap(data,nemsio_realkind,size(data))
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecvbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin8d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    type(nemsio_read_localdata) :: read_ldata

    if(present(iret)) iret=-42
!
   call nemsio_getgfile(gfile,read_ldata,iret)
    iskip=read_ldata%mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*read_ldata%myfieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrreadl(read_ldata%myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(read_ldata%do_byteswap) call byteswap(data,nemsio_dblekind,size(data))
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecbin8d8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr
    type(nemsio_read_localdata) :: read_ldata

    if(present(iret)) iret=-44
!
   call nemsio_getgfile(gfile,read_ldata,iret)
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=read_ldata%mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*read_ldata%myfieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrreadl(read_ldata%myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(read_ldata%do_byteswap) call byteswap(data,nemsio_dblekind,size(data))
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecvbin8d8
!------------------------------------------------------------------------------
!
!*****************   read w34 data set :  *************************************
!
!------------------------------------------------------------------------------
  subroutine  nemsio_readrecgrb4w34(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array,
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i,w34
    type(nemsio_read_localdata) :: read_ldata
!
!------------------------------------------------------------
! set up grib meta
!------------------------------------------------------------
    luidx=0
!
   call nemsio_getgfile(gfile,read_ldata,iret)
    if ( present(iret)) iret=-45
    w34=1
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    allocate(lbms(grbmeta%jf))
    N=0
!------------------------------------------------------------
! get data from getgb
!------------------------------------------------------------
    call getgbm(read_ldata%myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      read_ldata%mymbuf,read_ldata%mycbuf,read_ldata%mynlen,         &
      read_ldata%mynnum,read_ldata%mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine nemsio_readrecgrb4w34
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvgrb4w34(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into 32 bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character*(*),intent(in)                      :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)            :: vlev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i,w34
    type(nemsio_read_localdata) :: read_ldata
!
!------------------------------------------------------------
! set up grib meta
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-45
!
   call nemsio_getgfile(gfile,read_ldata,iret)
    w34=1
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev ,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w34
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(read_ldata%myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      read_ldata%mymbuf,read_ldata%mycbuf,read_ldata%mynlen,         &
      read_ldata%mynnum,read_ldata%mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine nemsio_readrecvgrb4w34
!------------------------------------------------------------------------------
!
!*****************   read grb data set w3d:  *************************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrecgrb8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 64 bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
    type(nemsio_read_localdata) :: read_ldata
!
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-46
!
   call nemsio_getgfile(gfile,read_ldata,iret)
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(read_ldata%myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      read_ldata%mymbuf,read_ldata%mycbuf,read_ldata%mynlen,         &
      read_ldata%mynnum,read_ldata%mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine nemsio_readrecgrb8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvgrb8(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character*(*),intent(in)                     :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)            :: vlev
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
    type(nemsio_read_localdata) :: read_ldata
!
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-47
!
   call nemsio_getgfile(gfile,read_ldata,iret)
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(read_ldata%myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      read_ldata%mymbuf,read_ldata%mycbuf,read_ldata%mynlen,         &
      read_ldata%mynnum,read_ldata%mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine nemsio_readrecvgrb8
!------------------------------------------------------------------------------
end module nemsio_read
