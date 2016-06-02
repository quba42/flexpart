!**********************************************************************
! Copyright 1998,1999,2000,2001,2002,2005,2007,2008,2009,2010         *
! Andreas Stohl, Petra Seibert, A. Frank, Gerhard Wotawa,             *
! Caroline Forster, Sabine Eckhardt, John Burkhart, Harald Sodemann   *
!                                                                     *
! This file is part of FLEXPART.                                      *
!                                                                     *
! FLEXPART is free software: you can redistribute it and/or modify    *
! it under the terms of the GNU General Public License as published by*
! the Free Software Foundation, either version 3 of the License, or   *
! (at your option) any later version.                                 *
!                                                                     *
! FLEXPART is distributed in the hope that it will be useful,         *
! but WITHOUT ANY WARRANTY; without even the implied warranty of      *
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
! GNU General Public License for more details.                        *
!                                                                     *
! You should have received a copy of the GNU General Public License   *
! along with FLEXPART.  If not, see <http://www.gnu.org/licenses/>.   *
!**********************************************************************

subroutine readwind_gfs(indj,n,uuh,vvh,wwh)

  !***********************************************************************
  !*                                                                     *
  !*             TRAJECTORY MODEL SUBROUTINE READWIND                    *
  !*                                                                     *
  !***********************************************************************
  !*                                                                     *
  !*             AUTHOR:      G. WOTAWA                                  *
  !*             DATE:        1997-08-05                                 *
  !*             LAST UPDATE: 2000-10-17, Andreas Stohl                  *
  !*             CHANGE: 01/02/2001, Bernd C. Krueger, Variables tth and *
  !*                     qvh (on eta coordinates) in common block        *
  !*             CHANGE: 16/11/2005, Caroline Forster, GFS data          *
  !*             CHANGE: 11/01/2008, Harald Sodemann, Input of GRIB1/2   *
  !*                     data with the ECMWF grib_api library            *
  !*             CHANGE: 03/12/2008, Harald Sodemann, update to f90 with *
  !*                                 ECMWF grib_api                      *
  !*                                                                     *
  !***********************************************************************
  !*                                                                     *
  !* DESCRIPTION:                                                        *
  !*                                                                     *
  !* READING OF ECMWF METEOROLOGICAL FIELDS FROM INPUT DATA FILES. THE   *
  !* INPUT DATA FILES ARE EXPECTED TO BE AVAILABLE IN GRIB CODE          *
  !*                                                                     *
  !***********************************************************************
  !  Changes Arnold, D. and Morton, D. (2015):                           *
  !   -- description of local and common variables                       *
  !***********************************************************************

  use grib_api
  use par_mod
  use com_mod
  use class_vtable

  implicit none

  !***********************************************************************
  ! Subroutine Parameters:                                               *
  !    input                                                             *
  ! indj                indicates number of the wind field to be read in *
  ! n                   temporal index for meteorological fields (1 to 3)*
  ! uuh,vvh, wwh        wind components in ECMWF model levels            *  
  integer :: indj,n
  real(kind=4) :: uuh(0:nxmax-1,0:nymax-1,nuvzmax)
  real(kind=4) :: vvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real(kind=4) :: wwh(0:nxmax-1,0:nymax-1,nwzmax)
  !***********************************************************************

  !***********************************************************************
  ! Local variables:                                                     *
  !                                                                      *
  ! HSO variables for grib_api:                                          *
  ! ifile               grib file to be opened and read in               *         
  ! iret                is a return code for successful or not open      *
  ! igrib               grib edition number (whether GRIB1 or GRIB2)     *
  ! gribVer             where info on igrib is kept, 1 for GRIB1 and     *
  !                     2 for GRIB2                                      *
  ! parCat              parameter category ( = number) , how FLEXPART    *
  !                     identifies fields                                *
  ! parNum              parameter number by product discipline and       *
  !                     parameter category                               *
  ! typSurf             type of first fixed surface                      *                   
  ! valSurf             level                                            *
  ! discipl             discipline of processed data contained in a      *
  !                     GRIB message                                     *
  integer :: ifile
  integer :: iret
  integer :: igrib
  integer :: gribVer,parCat,parNum,typSurf,valSurf,discipl
  !                                                                      *
  ! NCEP specific variables:                                             *
  ! numpt               number of pressure levels for temperature        *
  ! numpu               number of pressure levels for U velocity         *        
  ! numpv               number of pressure levels for V velocity         *
  ! numpw               number of pressure levels for W velocity         *
  ! numprh              number of pressure levels for relative humidity  *
  ! help                temporary variable to store fields               *
  ! temp                temporary variable to store tth and tt2 variables*
  ! ew                  function to calculate Goff-Gratch formula,       *
  !                     the saturation water vapor pressure              *
  ! elev                elevation in meters                              *
  ! ulev1               U velocity at the lowest sigma level             *
  ! vlev1               V velocity at the lowest sigma level             *
  ! tlev1               temperature at the lowest sigma level            *
  ! qvh2                2m dew point temperature                         *
  ! i179                number of pints in x                             *              
  ! i180                i179 +1, x direction +1                          *
  ! i181                i180 +1                                          *  
  integer :: numpt,numpu,numpv,numpw,numprh
  real :: help, temp, ew
  real :: elev
  real :: ulev1(0:nxmax-1,0:nymax-1),vlev1(0:nxmax-1,0:nymax-1)
  real :: tlev1(0:nxmax-1,0:nymax-1)
  real :: qvh2(0:nxmax-1,0:nymax-1)
  integer :: i179,i180,i181


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!! Vtable related variables

    ! Paths to Vtables (current implementation will assume they are in the cwd
    ! This is a crappy place for these parameters.  Need to move them.
    character(LEN=255), parameter :: VTABLE_NCEP_GRIB1_PATH = &
                                         "Vtable_ncep_grib1", &
                                     VTABLE_NCEP_GRIB2_PATH = &
                                         "Vtable_ncep_grib2"



    integer :: gribfile_type
    integer :: current_grib_level    ! This "was" isec1(8) in previous version
    character(len=255) :: gribfile_name
    character(len=255) :: vtable_path
    character(len=15) :: fpname

    type(Vtable) :: my_vtable    ! unallocated
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!









  !                                                                      *                                 
  ! VARIABLES AND ARRAYS NEEDED FOR GRIB DECODING                        *   
  !HSO kept isec1, isec2 and zsec4 for consistency with gribex GRIB input*
  !
  integer :: isec2(3)
  real(kind=4) :: zsec4(jpunp)
  real(kind=4) :: xaux,yaux,xaux0,yaux0
  real(kind=8) :: xauxin,yauxin
  real(kind=4) :: ewss(0:nxmax-1,0:nymax-1),nsss(0:nxmax-1,0:nymax-1)
  real :: plev1,hlev1,ff10m,fflev1
  logical :: hflswitch,strswitch
  !                                                                      *
  ! Other variables:                                                     *
  ! i,j,k               loop control indices in each direction           *
  ! levdiff2            number of model levels - number of model levels  *
  !                     for the staggered vertical wind +1               *                                    
  ! ifield              index to control field read in                   *
  ! iumax                                                                *
  ! iwmax                                                                *
  integer :: ii,i,j,k,levdiff2,ifield,iumax,iwmax
  !***********************************************************************

  !***********************************************************************
  ! Local constants                                                      *
  real,parameter :: eps=1.e-4
  !HSO  grib api error messages:
  character(len=24) :: gribErrorMsg = 'Error reading grib file'
  character(len=20) :: gribFunction = 'readwind_gfs'
  !***********************************************************************

  !***********************************************************************
  ! Global variables                                                     *
  !     from par_mod and com_mod                                         *
  ! wfname             File name of data to be read in                   *
  ! nx,ny,nuvz,nwz     expected field dimensions                         *
  ! nxfield            same as nx but for limited area fields            *
  ! nxmin1,nymin1           nx-1, ny-1, respectively                     *
  ! nxmax,nymax        maximum dimension of wind fields in x and y       *
  !                    direction, respectively                           *
  ! nuvzmax,nwzmax     maximum dimension of (u,v) and (w) wind fields in z
  !                    direction (for fields on eta levels)              *
  ! nlev_ec            number of vertical levels ecmwf model             *
  ! u10,v10            wind fields at 10 m                               *
  ! tt2,td2            temperature and dew point temperature at 2 m      *
  ! tth,qvh            temperature and specific humidity on original     *
  !                    eta levels                                        *
  ! ps                 surface pressure                                  *
  ! sd                 snow depth (but not used afterwards!)             *
  ! msl                mean sea level pressure                           *
  ! ttc                total cloud cover                                 *
  ! lsprec             large scale precipitation                         *
  ! convprec           convective precipitation                          *
  ! sshf               sensible heat flux                                *
  ! ssr                solar radiation                                   *
  ! surfstr              surface stress                                  *
  ! oro                orography                                         *
  ! excessoro          standard deviation of orography                   *
  ! lsm                land sea mask                                     *
  ! hmix               mixing height                                     *
  !***********************************************************************

!-----------------------------------------------------------------------------








  hflswitch=.false.
  strswitch=.false.
  levdiff2=nlev_ec-nwz+1
  iumax=0
  iwmax=0



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!  VTABLE code
    !!!!!!! Vtable choice
    gribfile_name = path(3)(1:length(3))//trim(wfname(indj))
    print *, 'gribfile_name: ', gribfile_name

    gribfile_type = vtable_detect_gribfile_type( gribfile_name )

    print *, 'gribfile_type: ', gribfile_type 

    if (gribfile_type .eq. VTABLE_GRIBFILE_TYPE_NCEP_GRIB1) then
        vtable_path = VTABLE_NCEP_GRIB1_PATH
    else if (gribfile_type .eq. VTABLE_GRIBFILE_TYPE_NCEP_GRIB2) then
        vtable_path = VTABLE_NCEP_GRIB2_PATH
    else
        print *, 'Unsupported gribfile_type: ', gribfile_type
        stop
    endif


    ! Load the Vtable into 'my_vtable'
    print *, 'Loading Vtable: ', vtable_path
    call vtable_load_by_name(vtable_path, my_vtable)
    print *, 'Vtable Initialized: ', my_vtable%initialized
    print *, 'Vtable num_entries: ', my_vtable%num_entries
    !!!!!!!!!!!!!!!!!!!  VTABLE code
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  ! OPENING OF DATA FILE (GRIB CODE)

  !HSO
5   call grib_open_file(ifile,path(3)(1:length(3)) &
         //trim(wfname(indj)),'r',iret)
  if (iret.ne.GRIB_SUCCESS) then
    goto 888   ! ERROR DETECTED
  endif
  !turn on support for multi fields messages
  call grib_multi_support_on

  numpt=0
  numpu=0
  numpv=0
  numpw=0
  numprh=0
  ifield=0
10   ifield=ifield+1
  !
  ! GET NEXT FIELDS
  !
  call grib_new_from_file(ifile,igrib,iret)
  if (iret.eq.GRIB_END_OF_FILE)  then
    goto 50    ! EOF DETECTED
  elseif (iret.ne.GRIB_SUCCESS) then
    goto 888   ! ERROR DETECTED
  endif


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!  VTABLE code
    ! Get the fpname
    fpname = vtable_get_fpname(igrib, my_vtable)
    !print *, 'fpname: ', trim(fpname)


    !!!!!!!!!!!!!!!!!!!  VTABLE code
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !first see if we read GRIB1 or GRIB2
  call grib_get_int(igrib,'editionNumber',gribVer,iret)
  call grib_check(iret,gribFunction,gribErrorMsg)

  if (gribVer.eq.1) then ! GRIB Edition 1

      call grib_get_int(igrib,'level', current_grib_level, iret)
      call grib_check(iret,gribFunction,gribErrorMsg)

      !!! Added by DJM 2016-03-02 - if this is GRIB1 we assume that
      !!! level units are hPa and need to be multiplied by 100 for Pa
      current_grib_level = current_grib_level*100.0

  else ! GRIB Edition 2
  
      call grib_get_int(igrib, 'scaledValueOfFirstFixedSurface', &
           current_grib_level, iret)      
      call grib_check(iret,gribFunction,gribErrorMsg)

  endif ! gribVer

  if (trim(fpname) .ne. 'None') then
  !  get the size and data of the values array
    call grib_get_real4_array(igrib,'values',zsec4,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
  endif

  if(ifield.eq.1) then

  !get the required fields from section 2
  !store compatible to gribex input
  call grib_get_int(igrib,'numberOfPointsAlongAParallel', &
       isec2(2),iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_int(igrib,'numberOfPointsAlongAMeridian', &
       isec2(3),iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_real8(igrib,'longitudeOfFirstGridPointInDegrees', &
       xauxin,iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_real8(igrib,'latitudeOfLastGridPointInDegrees', &
       yauxin,iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  xaux=xauxin+real(nxshift)*dx
  !print *, 'xauxin: ', xauxin
  !print *, 'nxshift: ', nxshift
  !print *, 'dx: ', dx
  !print *, 'xlon0: ', xlon0
  yaux=yauxin

  ! CHECK GRID SPECIFICATIONS

    if(isec2(2).ne.nxfield) stop 'READWIND: NX NOT CONSISTENT'
    if(isec2(3).ne.ny) stop 'READWIND: NY NOT CONSISTENT'

    if(xaux.eq.0.) xaux=-179.0     ! NCEP GRIB DATA
    xaux0=xlon0
    yaux0=ylat0
    if(xaux.lt.0.) xaux=xaux+360.
    if(yaux.lt.0.) yaux=yaux+360.
    if(xaux0.lt.0.) xaux0=xaux0+360.
    if(yaux0.lt.0.) yaux0=yaux0+360.
    if(abs(xaux-xaux0).gt.eps) &
         stop 'READWIND: LOWER LEFT LONGITUDE NOT CONSISTENT'
    if(abs(yaux-yaux0).gt.eps) &
         stop 'READWIND: LOWER LEFT LATITUDE NOT CONSISTENT'
  endif
  !HSO end of edits

  i179=nint(179./dx)
  if (dx.lt.0.7) then
    i180=nint(180./dx)+1    ! 0.5 deg data
  else
    i180=nint(179./dx)+1    ! 1 deg data
  endif
  i181=i180+1

!!!!  if (trim(fpname) .ne. 'None') then


  ! TEMPERATURE
  if( trim(fpname) .eq. 'TT' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              if((i.eq.0).and.(j.eq.0)) then
                  do ii=1,nuvz
                      if (current_grib_level .eq. akz(ii)) numpt=ii
                  end do
              endif
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  tth(i179+i,j,numpt,n)=help
              else
                  tth(i-i181,j,numpt,n)=help
              endif
          end do
      end do
  endif   


  ! U VELOCITY
  if( trim(fpname) .eq. 'UU' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              if((i.eq.0).and.(j.eq.0)) then
                  do ii=1,nuvz
                      if (current_grib_level .eq. akz(ii)) numpu=ii
                  end do
              endif
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  uuh(i179+i,j,numpu)=help
              else
                  uuh(i-i181,j,numpu)=help
              endif
          end do
      end do
  endif   

  ! V VELOCITY
  if( trim(fpname) .eq. 'VV' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              if((i.eq.0).and.(j.eq.0)) then
                  do ii=1,nuvz
                      if (current_grib_level .eq. akz(ii)) numpv=ii
                  end do
              endif
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  vvh(i179+i,j,numpv)=help
              else
                  vvh(i-i181,j,numpv)=help
              endif
          end do
      end do
  endif   


  ! W VELOCITY
  if( trim(fpname) .eq. 'WW' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              if((i.eq.0).and.(j.eq.0)) then
                  do ii=1,nuvz
                      if (current_grib_level .eq. akz(ii)) numpw=ii
                  end do
              endif
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  wwh(i179+i,j,numpw)=help
              else
                  wwh(i-i181,j,numpw)=help
              endif
          end do
      end do
  endif   


  ! RELATIVE HUMIDITY -> CONVERT TO SPECIFIC HUMIDITY LATER
  if( trim(fpname) .eq. 'RH' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              if((i.eq.0).and.(j.eq.0)) then
                  do ii=1,nuvz
                      if (current_grib_level .eq. akz(ii)) numprh=ii
                  end do
              endif
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  qvh(i179+i,j,numprh,n)=help
              else
                  qvh(i-i181,j,numprh,n)=help
              endif
          end do
      end do
  endif   


  ! SURFACE PRESSURE
  if( trim(fpname) .eq. 'PS' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  ps(i179+i,j,1,n)=help
              else
                  ps(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! SNOW DEPTH
  if( trim(fpname) .eq. 'SD' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  sd(i179+i,j,1,n)=help
              else
                  sd(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! MEAN SEA LEVEL PRESSURE
  if( trim(fpname) .eq. 'SLP' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  msl(i179+i,j,1,n)=help
              else
                  msl(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! TOTAL CLOUD COVER
  if( trim(fpname) .eq. 'TCC' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  tcc(i179+i,j,1,n)=help
              else
                  tcc(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      
  ! 10 M U VELOCITY
  if( trim(fpname) .eq. 'U10' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  u10(i179+i,j,1,n)=help
              else
                  u10(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      
  ! 10 M V VELOCITY
  if( trim(fpname) .eq. 'V10' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  v10(i179+i,j,1,n)=help
              else
                  v10(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! 2 M TEMPERATURE
  if( trim(fpname) .eq. 'T2' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  tt2(i179+i,j,1,n)=help
              else
                  tt2(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      
  ! 2 M DEW POINT TEMPERATURE
  if( trim(fpname) .eq. 'TD2' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  td2(i179+i,j,1,n)=help
              else
                  td2(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! LARGE SCALE PREC.
  if( trim(fpname) .eq. 'LSPREC' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  lsprec(i179+i,j,1,n)=help
              else
                  lsprec(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! CONVECTIVE PREC.
  if( trim(fpname) .eq. 'CONVPREC' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  convprec(i179+i,j,1,n)=help
              else
                  convprec(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! TOPOGRAPHY
  if( trim(fpname) .eq. 'ORO' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  oro(i179+i,j)=help
                  excessoro(i179+i,j)=0.0 ! ISOBARIC SURFACES: SUBGRID 
                                          ! TERRAIN DISREGARDED
              else
                  oro(i-i181,j)=help
                  excessoro(i-i181,j)=0.0 ! ISOBARIC SURFACES: SUBGRID 
                                          ! TERRAIN DISREGARDED
              endif
          end do
      end do
  endif
      
  ! LAND SEA MASK
  if( trim(fpname) .eq. 'LSM' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  lsm(i179+i,j)=help
              else
                  lsm(i-i181,j)=help
              endif
          end do
      end do
  endif
      

  ! MIXING HEIGHT
  if( trim(fpname) .eq. 'HMIX' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  hmix(i179+i,j,1,n)=help
              else
                  hmix(i-i181,j,1,n)=help
              endif
          end do
      end do
  endif
      

  ! 2 M RELATIVE HUMIDITY
  if( trim(fpname) .eq. 'RH2' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  qvh2(i179+i,j)=help
              else
                  qvh2(i-i181,j)=help
              endif
          end do
      end do
  endif
      

  ! TEMPERATURE LOWEST SIGMA LEVEL
  if( trim(fpname) .eq. 'TSIG1' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  tlev1(i179+i,j)=help
              else
                  tlev1(i-i181,j)=help
              endif
          end do
      end do
  endif
      

  ! U VELOCITY LOWEST SIGMA LEVEL
  if( trim(fpname) .eq. 'USIG1' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  ulev1(i179+i,j)=help
              else
                  ulev1(i-i181,j)=help
              endif
          end do
      end do
  endif
      
  ! V VELOCITY LOWEST SIGMA LEVEL
  if( trim(fpname) .eq. 'VSIG1' ) then
      do j=0,nymin1
          do i=0,nxfield-1
              help=zsec4(nxfield*(ny-j-1)+i+1)
              if(i.le.i180) then
                  vlev1(i179+i,j)=help
              else
                  vlev1(i-i181,j)=help
              endif
          end do
      end do
  endif
      




!!!!  endif

  if( trim(fpname) .eq. 'UU') then
  ! NCEP ISOBARIC LEVELS - this counts the number of pressure levels
  ! by counting the number of occurrences of UU, which us u-wind on pressure levels
    iumax=iumax+1
  endif

  call grib_release(igrib)
  goto 10                      !! READ NEXT LEVEL OR PARAMETER
  !
  ! CLOSING OF INPUT DATA FILE
  !

  !HSO close grib file
50   continue
  call grib_close_file(ifile)

  ! SENS. HEAT FLUX
  sshf(:,:,1,n)=0.0     ! not available from gfs.tccz.pgrbfxx files
  hflswitch=.false.    ! Heat flux not available
  ! SOLAR RADIATIVE FLUXES
  ssr(:,:,1,n)=0.0      ! not available from gfs.tccz.pgrbfxx files
  ! EW SURFACE STRESS
  ewss=0.0         ! not available from gfs.tccz.pgrbfxx files
  ! NS SURFACE STRESS
  nsss=0.0         ! not available from gfs.tccz.pgrbfxx files
  strswitch=.false.    ! stress not available

  ! CONVERT TP TO LSP (GRIB2 only)
  if (gribVer.eq.2) then
    do j=0,nymin1
    do i=0,nxfield-1
     if(i.le.i180) then
     if (convprec(i179+i,j,1,n).lt.lsprec(i179+i,j,1,n)) then ! neg precip would occur
         lsprec(i179+i,j,1,n)= &
              lsprec(i179+i,j,1,n)-convprec(i179+i,j,1,n)
     else
         lsprec(i179+i,j,1,n)=0
     endif
     else
     if (convprec(i-i181,j,1,n).lt.lsprec(i-i181,j,1,n)) then
          lsprec(i-i181,j,1,n)= &
               lsprec(i-i181,j,1,n)-convprec(i-i181,j,1,n)
     else
          lsprec(i-i181,j,1,n)=0
     endif
     endif
    enddo
    enddo
  endif
  !HSO end edits


  ! TRANSFORM RH TO SPECIFIC HUMIDITY

  do j=0,ny-1
    do i=0,nxfield-1
      do k=1,nuvz
        help=qvh(i,j,k,n)
        temp=tth(i,j,k,n)
        plev1=akm(k)+bkm(k)*ps(i,j,1,n)
        elev=ew(temp)*help/100.0
        qvh(i,j,k,n)=xmwml*(elev/(plev1-((1.0-xmwml)*elev)))
      end do
    end do
  end do

  ! CALCULATE 2 M DEW POINT FROM 2 M RELATIVE HUMIDITY
  ! USING BOLTON'S (1980) FORMULA
  ! BECAUSE td2 IS NOT AVAILABLE FROM NCEP GFS DATA

  do j=0,ny-1
    do i=0,nxfield-1
        help=qvh2(i,j)
        temp=tt2(i,j,1,n)
        elev=ew(temp)/100.*help/100.   !vapour pressure in hPa
        td2(i,j,1,n)=243.5/(17.67/log(elev/6.112)-1)+273.
        if (help.le.0.) td2(i,j,1,n)=tt2(i,j,1,n)
    end do
  end do

  if(levdiff2.eq.0) then
    iwmax=nlev_ec+1
    do i=0,nxmin1
      do j=0,nymin1
        wwh(i,j,nlev_ec+1)=0.
      end do
    end do
  endif


  ! For global fields, assign the leftmost data column also to the rightmost
  ! data column; if required, shift whole grid by nxshift grid points
  !*************************************************************************

  if (xglobal) then
    call shift_field_0(ewss,nxfield,ny)
    call shift_field_0(nsss,nxfield,ny)
    call shift_field_0(oro,nxfield,ny)
    call shift_field_0(excessoro,nxfield,ny)
    call shift_field_0(lsm,nxfield,ny)
    call shift_field_0(ulev1,nxfield,ny)
    call shift_field_0(vlev1,nxfield,ny)
    call shift_field_0(tlev1,nxfield,ny)
    call shift_field_0(qvh2,nxfield,ny)
    call shift_field(ps,nxfield,ny,1,1,2,n)
    call shift_field(sd,nxfield,ny,1,1,2,n)
    call shift_field(msl,nxfield,ny,1,1,2,n)
    call shift_field(tcc,nxfield,ny,1,1,2,n)
    call shift_field(u10,nxfield,ny,1,1,2,n)
    call shift_field(v10,nxfield,ny,1,1,2,n)
    call shift_field(tt2,nxfield,ny,1,1,2,n)
    call shift_field(td2,nxfield,ny,1,1,2,n)
    call shift_field(lsprec,nxfield,ny,1,1,2,n)
    call shift_field(convprec,nxfield,ny,1,1,2,n)
    call shift_field(sshf,nxfield,ny,1,1,2,n)
    call shift_field(ssr,nxfield,ny,1,1,2,n)
    call shift_field(hmix,nxfield,ny,1,1,2,n)
    call shift_field(tth,nxfield,ny,nuvzmax,nuvz,2,n)
    call shift_field(qvh,nxfield,ny,nuvzmax,nuvz,2,n)
    call shift_field(uuh,nxfield,ny,nuvzmax,nuvz,1,1)
    call shift_field(vvh,nxfield,ny,nuvzmax,nuvz,1,1)
    call shift_field(wwh,nxfield,ny,nwzmax,nwz,1,1)
  endif

  do i=0,nxmin1
    do j=0,nymin1
  ! Convert precip. from mm/s -> mm/hour
      convprec(i,j,1,n)=convprec(i,j,1,n)*3600.
      lsprec(i,j,1,n)=lsprec(i,j,1,n)*3600.
      surfstr(i,j,1,n)=sqrt(ewss(i,j)**2+nsss(i,j)**2)
    end do
  end do

  if ((.not.hflswitch).or.(.not.strswitch)) then
  !  write(*,*) 'WARNING: No flux data contained in GRIB file ',
  !    +  wfname(indj)

  ! CALCULATE USTAR AND SSHF USING THE PROFILE METHOD
  !***************************************************************************

    do i=0,nxmin1
      do j=0,nymin1
        hlev1=30.0                     ! HEIGHT OF FIRST MODEL SIGMA LAYER
        ff10m= sqrt(u10(i,j,1,n)**2+v10(i,j,1,n)**2)
        fflev1=sqrt(ulev1(i,j)**2+vlev1(i,j)**2)
        call pbl_profile(ps(i,j,1,n),td2(i,j,1,n),hlev1, &
             tt2(i,j,1,n),tlev1(i,j),ff10m,fflev1, &
             surfstr(i,j,1,n),sshf(i,j,1,n))
        if(sshf(i,j,1,n).gt.200.) sshf(i,j,1,n)=200.
        if(sshf(i,j,1,n).lt.-400.) sshf(i,j,1,n)=-400.
      end do
    end do
  endif


  if(iumax.ne.nuvz) stop 'READWIND: NUVZ NOT CONSISTENT'
  if(iumax.ne.nwz)    stop 'READWIND: NWZ NOT CONSISTENT'

  return
888   write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
  write(*,*) ' #### ',wfname(indj),'                    #### '
  write(*,*) ' #### IS NOT GRIB FORMAT !!!                  #### '
  stop 'Execution terminated'
999   write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
  write(*,*) ' #### ',wfname(indj),'                    #### '
  write(*,*) ' #### CANNOT BE OPENED !!!                    #### '
  stop 'Execution terminated'

end subroutine readwind_gfs
