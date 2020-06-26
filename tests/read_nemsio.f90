!- - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
! the program gives some examples of using nemsio modules
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- -
! --
  program main
!
  use nemsio_module
  implicit none
!
  integer, parameter:: double=selected_real_kind(p=13,r=200)
  type(nemsio_gfile) :: gfile,gfilem2,gfilem3,gfiled2
!
  real (kind=8) timef
  character(255) cin
  character(8) gdatatype,modelname
  character(2) level
  real,allocatable  :: tmp(:),data1(:,:)
!---------------------------------------------------------------------------
!--- nemsio meta data
  real  isecond,pdtop,stime,etime,dummy,degrad,wind10m,windtmp
  integer nrec,im,jm,lm,l,idate(7),version, im2,jm2, nframe, &
          ntrac,irealf,nrec1,version1,nmeta1,nfhour,nfminute,nfsecond, &
          nfsecondn,nfsecondd,num3d,num2d,nmetavari,nmetavarr,nmetavarl, &
          nmetaaryi,nmetaaryr,nmeta,tlmeta
  integer ihrst,idat(3),mp_physics,sf_surface_physics,icycle,fieldsize
  logical global, run,extrameta
  character(16),allocatable:: recname(:),metaaryrname(:)
  character(16),allocatable  :: reclevtyp(:)
  integer,allocatable:: reclev(:),metaaryrlen(:)
  character(16),allocatable:: recname1(:)
  character(16),allocatable  :: reclevtyp1(:)
  integer,allocatable:: reclev1(:)
  real,allocatable       :: metaaryrval(:,:),cpi(:),ri(:)
!---------------------------------------------------------------------------
!--- local vars
  character(16) vname
  character(32) gtype
  character(16) vlevtyp
  integer i,ii,j,jj,jrec,krec,vlev,iret,lev,ista,iend,jsta,jend
!---------------------------------------------------------------------------
!--- NMM vars
  integer NUNIT, NSOIL,irec
!
  character(8),allocatable :: variname(:),varrname(:),varlname(:),aryiname(:),aryrname(:)
  integer,allocatable :: varival(:),aryilen(:),aryrlen(:),aryival(:,:)
  real,allocatable :: varrval(:),aryrval(:,:)
  logical,allocatable :: varlval(:)
!
  NSOIL=4
  NUNIT=20
  degrad=90./asin(1.)
!
!---------------------------------------------------------------------------
!
!-------------set up nemsio write--------------------------
  call nemsio_init(iret=iret)
  print *,'nemsio_init, iret=',iret
!
!+++++++++++++++++ read nemsil file with 2 meta data
!+++++++++++++++++++++++++++
!
!--- open gfile for reading
  print *,'3b:: start reading nemsio file '
!  cin='nemsio_2meta_big'
  call getarg(1,cin)
  call nemsio_open(gfile,trim(cin),'read',iret=iret)
  if(iret/=0) print *,'3b:: after open read, ',trim(cin), ' iret=',iret
!
!--- get dimension
  im=0;jm=0;lm=0;nframe=0;nrec=0
  call nemsio_getfilehead(gfile,dimx=im,dimy=jm,dimz=lm,nframe=nframe,nrec=nrec,&
       gdatatype=gdatatype,modelname=modelname,nmeta=nmeta,ntrac=ntrac,tlmeta=tlmeta,iret=iret)
  print *,'3b:: gfilem2,im=',im,'jm=',jm,'lm=',lm,'nframe=',nframe,'nrec=',nrec, &
       'gdatatype=',gdatatype,' modelname=',modelname,' nmeta=',nmeta,'ntrac=',ntrac, &
       'iret=',iret
  fieldsize=(im+2*nframe)*(jm+2*nframe)
!
  call nemsio_getfilehead(gfile,nmetavari=nmetavari,nmetavarr=nmetavarr,nmetavarl=nmetavarl, &
       nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr)
    print *,'nmetavari=',nmetavari,'nmetavarr=',nmetavarr,'nmetavarl=',nmetavarl
  if(nmetavari>0) then
    allocate(variname(nmetavari),varival(nmetavari))
    call nemsio_getfilehead(gfile,variname=variname,varival=varival)
    print *,'variname=',variname,'varival=',varival
  endif
  if(nmetavarr>0) then
    allocate(varrname(nmetavarr),varrval(nmetavarr))
    call nemsio_getfilehead(gfile,varrname=varrname,varrval=varrval)
    print *,'varrname=',varrname,'varrval=',varrval

  endif
  if(nmetavarl>0) then
    allocate(varlname(nmetavarl),varlval(nmetavarl))
    call nemsio_getfilehead(gfile,varlname=varlname,varlval=varlval)
    print *,'varlname=',varlname,'varlval=',varlval
  endif
  if(nmetaaryi>0) then
    allocate(aryiname(nmetaaryi),aryilen(nmetaaryi))
    call nemsio_getfilehead(gfile,aryiname=aryiname,aryilen=aryilen)
    print *,'aryiname=',aryiname,'aryilen=',aryilen
    allocate(aryival(maxval(aryilen),nmetaaryi))
    call nemsio_getfilehead(gfile,aryival=aryival)
    do i=1,nmetaaryi
      print *,'aryiname=',aryiname(i),aryilen(i),aryival(1:aryilen(i),i)
    enddo
  endif
  if(nmetaaryr>0) then
    allocate(aryrname(nmetaaryr),aryrlen(nmetaaryr))
    call nemsio_getfilehead(gfile,aryrname=aryrname,aryrlen=aryrlen)
    print *,'aryrname=',aryrname,'aryrlen=',aryrlen
    allocate(aryrval(maxval(aryrlen),nmetaaryr))
    call nemsio_getfilehead(gfile,aryrval=aryrval)
    do i=1,nmetaaryr
      print *,'aryrname=',aryrname(i),aryrlen(i),aryrval(1:aryrlen(i),i)
    enddo
  endif

!
!---read 2 fields:tmp and hgt
  allocate(tmp(fieldsize),data1(im+2*nframe,jm+2*nframe))
  tmp=0.
  do jrec=1,nrec
    call nemsio_getrechead(gfile,jrec,vname,vlevtyp,vlev,iret)
    call nemsio_readrec(gfile,jrec,tmp,iret=iret)
    print *,'3b:: read,jrec=',jrec,'iret=',iret,' vname=',trim(vname), &
       ' vlevtyp=',trim(vlevtyp),' vlev=',vlev,'data1=',maxval(tmp),minval(tmp)
   enddo
!
!--- close nemsio file
   call nemsio_close(gfile,iret=iret)
  if ( iret .ne.0) print *,'iret=',iret
  print *,'****   after read 2 meta files! ****'

!!---------------------------------------------------------------------------
  deallocate(tmp,data1)
!
!---------------------------------------------------------------------------
!
  call nemsio_finalize()
  print *,'nemsio final'
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- -
! --
  stop

 end program

