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

subroutine readwind_ecmwf(indj,n,uuh,vvh,wwh)

  !**********************************************************************
  !                                                                     *
  !             TRAJECTORY MODEL SUBROUTINE READWIND                    *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  !             AUTHOR:      G. WOTAWA                                  *
  !             DATE:        1997-08-05                                 *
  !             LAST UPDATE: 2000-10-17, Andreas Stohl                  *
  !             CHANGE: 11/01/2008, Harald Sodemann, GRIB1/2 input with *
  !                                 ECMWF grib_api                      *
  !             CHANGE: 03/12/2008, Harald Sodemann, update to f90 with *
  !                                 ECMWF grib_api                      *
  !                                                                     *
  !**********************************************************************
  !  Changes, Bernd C. Krueger, Feb. 2001:
  !   Variables tth and qvh (on eta coordinates) in common block
  !**********************************************************************
  !                                                                     *
  ! DESCRIPTION:                                                        *
  !                                                                     *
  ! READING OF ECMWF METEOROLOGICAL FIELDS FROM INPUT DATA FILES. THE   *
  ! INPUT DATA FILES ARE EXPECTED TO BE AVAILABLE IN GRIB CODE          *
  !                                                                     *
  !**********************************************************************
  !  Changes Arnold, D. and Morton, D. (2015):                          *
  !   -- description of local and common variables                      *
  !**********************************************************************

  use GRIB_API
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
  ! Local variables                                                      *
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
#if defined CTBTO
  integer :: parId
#endif
  integer :: gotGrid


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!! Vtable related variables

    ! Paths to Vtables (current implementation will assume they are in the cwd
    ! This is a crappy place for these parameters.  Need to move them.
    character(LEN=255), parameter :: VTABLE_ECMWF_GRIB1_PATH = &
                                         "Vtable_ecmwf_grib1", &
                                     VTABLE_ECMWF_GRIB2_PATH = &
                                         "Vtable_ecmwf_grib2", &
                                     VTABLE_ECMWF_GRIB1_2_PATH = &
                                         "Vtable_ecmwf_grib1_2"

    integer :: gribfile_type
    integer :: current_grib_level    ! This "was" isec1(8) in previous version
    character(len=255) :: gribfile_name
    character(len=255) :: vtable_path
    character(len=15) :: fpname

    type(Vtable) :: my_vtable    ! unallocated
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !
  ! Variables and arrays for grib decoding:                              * 
  ! dimension of isec2 at least (22+n), where n is the number of         *
  ! parallels or meridians in a quasi-regular (reduced) Gaussian or      *
  ! lat/long grid                                                        *
  ! dimension of zsec2 at least (10+nn), where nn is the number of       *
  ! vertical coordinate parameters                                       *
  !                                                                      *
  ! isec1               grib definition section (version, center, ...    *
  ! isec2               grid description section                         *
  ! zsec4               the binary data section                          *    
  ! xaux                
  ! yaux
  ! xauxin
  ! yauxin
  ! xaux0               auxiliary variable for xlon0                     *
  ! yaux0               auxiliary variable for xlat0                     *
  ! nsss                NS surface stress                                *  
  ! ewss                EW surface stress                                *
  ! plev1               pressure of the first model layer                *
  ! pmean               mean pressure between ??                         *
  ! tv                  virtual temperature                              *
  ! fu                  for conversion from pressure to meters           *
  ! hlev1               height of the first model layer                  *
  ! ff10m               wind speed at 10 m                               *
  ! fflev1              wind speed at the first model layere             *
  ! hflswitch           logical variable to check existence of flux data *
  ! strswitch           logical variable to check existence of stress    *
  !                     data                                             *

  integer :: isec1(56),isec2(22+nxmax+nymax)
  real(kind=4) :: zsec4(jpunp)
  real(kind=4) :: xaux,yaux,xaux0,yaux0
#if defined CTBTO
  real(kind=4) :: conversion_factor
#endif
  real(kind=8) :: xauxin,yauxin
  real(kind=4) :: nsss(0:nxmax-1,0:nymax-1),ewss(0:nxmax-1,0:nymax-1)
  real :: plev1,pmean,tv,fu,hlev1,ff10m,fflev1
  logical :: hflswitch,strswitch

  ! Other variables:
  ! i,j,k               loop control indices in each direction           *
  ! levdiff2            number of model levels - number of model levels  *
  !                     for the staggered vertical wind +1               *                                    
  ! ifield              index to control field read in                   *
  ! iumax
  ! iwmax
  integer :: i,j,k,levdiff2,ifield,iumax,iwmax
  !***********************************************************************

  !***********************************************************************
  ! Local constants                                                      *
  real,parameter :: eps=1.e-4
  !HSO  grib api error messages:
  character(len=24) :: gribErrorMsg = 'Error reading grib file'
  character(len=20) :: gribFunction = 'readwind'
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
  ! r_air              individual gas constant for dry air [J/kg/K]      *
  ! ga                 gravity acceleration of earth [m/s**2]            *
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
    !print *, 'gribfile_name: ', gribfile_name

    gribfile_type = vtable_detect_gribfile_type( gribfile_name )

    !print *, 'gribfile_type: ', gribfile_type 

    if (gribfile_type .eq. VTABLE_GRIBFILE_TYPE_ECMWF_GRIB1) then
        vtable_path = VTABLE_ECMWF_GRIB1_PATH
    else if (gribfile_type .eq. VTABLE_GRIBFILE_TYPE_ECMWF_GRIB2) then
        vtable_path = VTABLE_ECMWF_GRIB2_PATH
    else if (gribfile_type .eq. VTABLE_GRIBFILE_TYPE_ECMWF_GRIB1_2) then
        vtable_path = VTABLE_ECMWF_GRIB1_2_PATH
    else
        print *, 'Unsupported gribfile_type: ', gribfile_type
        stop
    endif


    ! Load the Vtable into 'my_vtable'
    !print *, 'Loading Vtable: ', vtable_path
    call vtable_load_by_name(vtable_path, my_vtable)
    !print *, 'Vtable Initialized: ', my_vtable%initialized
    !print *, 'Vtable num_entries: ', my_vtable%num_entries
    !!!!!!!!!!!!!!!!!!!  VTABLE code
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





  !
  ! OPENING OF DATA FILE (GRIB CODE)
  !
5   call grib_open_file(ifile,path(3)(1:length(3)) &
         //trim(wfname(indj)),'r',iret)
  if (iret.ne.GRIB_SUCCESS) then
    goto 888   ! ERROR DETECTED
  endif
  !turn on support for multi fields messages */
  !call grib_multi_support_on

  gotGrid=0
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

#if defined CTBTO
  conversion_factor=1.0
#endif

    !first see if we read GRIB1 or GRIB2
    call grib_get_int(igrib,'editionNumber',gribVer,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)

    !!!!!!!  DJM - candidate for consolidation
    if (gribVer.eq.1) then ! GRIB Edition 1

        call grib_get_int(igrib,'level', current_grib_level,iret)
        call grib_check(iret,gribFunction,gribErrorMsg)


    else   ! GRIB Edition 2

#if defined CTBTO
        !print *, 'fpname: ', fpname
        call grib2check(igrib, fpname, conversion_factor)
#endif

        call grib_get_int(igrib,'level', current_grib_level,iret)
        call grib_check(iret,gribFunction,gribErrorMsg)


    endif   ! END IF Grib Version selection



  !HSO  get the size and data of the values array
  if (trim(fpname) .ne. 'None') then
    call grib_get_real4_array(igrib,'values',zsec4,iret)
#if defined CTBTO
    zsec4=zsec4/conversion_factor
#endif
    call grib_check(iret,gribFunction,gribErrorMsg)
  endif

  !HSO  get the required fields from section 2 in a gribex compatible manner
  if (ifield.eq.1) then
  call grib_get_int(igrib,'numberOfPointsAlongAParallel', &
       isec2(2),iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_int(igrib,'numberOfPointsAlongAMeridian', &
       isec2(3),iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_int(igrib,'numberOfVerticalCoordinateValues', &
       isec2(12))
  call grib_check(iret,gribFunction,gribErrorMsg)
  ! CHECK GRID SPECIFICATIONS
  if(isec2(2).ne.nxfield) stop 'READWIND: NX NOT CONSISTENT'
  if(isec2(3).ne.ny) stop 'READWIND: NY NOT CONSISTENT'
  if(isec2(12)/2-1.ne.nlev_ec) &
       stop 'READWIND: VERTICAL DISCRETIZATION NOT CONSISTENT'
  endif ! ifield

  !HSO  get the second part of the grid dimensions only from GRiB1 messages
#if defined CTBTO
if (gotGrid.eq.0) then
#else
!!! DJM 2016-05-29 -- this check for gribVer doesn't make sense, and causes
!!! failure with pure GRIB2, so I'm commenting out
!if ((gribVer.eq.1).and.(gotGrid.eq.0)) then
if (gotGrid.eq.0) then
#endif
    call grib_get_real8(igrib,'longitudeOfFirstGridPointInDegrees', &
         xauxin,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_real8(igrib,'latitudeOfLastGridPointInDegrees', &
         yauxin,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    xaux=xauxin+real(nxshift)*dx
    yaux=yauxin
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
    gotGrid=1
endif ! gotGrid


k = current_grib_level

!! TEMPERATURE
if(trim(fpname) .eq. 'TT') then
    do j=0,nymin1
        do i=0,nxfield-1
        tth(i,j,nlev_ec-k+2,n) = zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif


!! U VELOCITY
if(trim(fpname) .eq. 'UU') then
    iumax=max(iumax,nlev_ec-k+1)
    do j=0,nymin1
        do i=0,nxfield-1
        uuh(i,j,nlev_ec-k+2) = zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif


!! V VELOCITY
if(trim(fpname) .eq. 'VV') then
    do j=0,nymin1
        do i=0,nxfield-1
        vvh(i,j,nlev_ec-k+2) = zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! W VELOCITY
if(trim(fpname) .eq. 'ETADOT') then
    iwmax=max(iwmax,nlev_ec-k+1)
    do j=0,nymin1
        do i=0,nxfield-1
        wwh(i,j,nlev_ec-k+1) = zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! SPEC HUMIDITY
if(trim(fpname) .eq. 'QV') then
    do j=0,nymin1
        do i=0,nxfield-1
            qvh(i,j,nlev_ec-k+2,n)=zsec4(nxfield*(ny-j-1)+i+1)
            if (qvh(i,j,nlev_ec-k+2,n) .lt. 0.) qvh(i,j,nlev_ec-k+2,n) = 0.
            ! this is necessary because the gridded data may contain
            ! spurious negative values
        enddo
    enddo
endif

!! SURF. PRESS.
if(trim(fpname) .eq. 'PS') then
    do j=0,nymin1
        do i=0,nxfield-1
            ps(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! SNOW DEPTH
if(trim(fpname) .eq. 'SD') then
    do j=0,nymin1
        do i=0,nxfield-1
            sd(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! SEA LEVEL PRESS.
if(trim(fpname) .eq. 'MSL') then
    do j=0,nymin1
        do i=0,nxfield-1
            msl(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! CLOUD COVER
if(trim(fpname) .eq. 'TCC') then
    do j=0,nymin1
        do i=0,nxfield-1
            tcc(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! 10 M U VELOCITY
if(trim(fpname) .eq. 'U10') then
    do j=0,nymin1
        do i=0,nxfield-1
            u10(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! 10 M V VELOCITY
if(trim(fpname) .eq. 'V10') then
    do j=0,nymin1
        do i=0,nxfield-1
            v10(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! 2 M TEMPERATURE
if(trim(fpname) .eq. 'T2') then
    do j=0,nymin1
        do i=0,nxfield-1
            tt2(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! 2 M DEW POINT
if(trim(fpname) .eq. 'TD2') then
    do j=0,nymin1
        do i=0,nxfield-1
            td2(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! LARGE SCALE PREC.
if(trim(fpname) .eq. 'LSPREC') then
    do j=0,nymin1
        do i=0,nxfield-1
            lsprec(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
            if (lsprec(i,j,1,n).lt.0.) lsprec(i,j,1,n)=0.
        enddo
    enddo
endif

!! CONVECTIVE PREC.
if(trim(fpname) .eq. 'CONVPREC') then
    do j=0,nymin1
        do i=0,nxfield-1
            convprec(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
            if (convprec(i,j,1,n).lt.0.) convprec(i,j,1,n)=0.
        enddo
    enddo
endif

!! SENS. HEAT FLUX
if(trim(fpname) .eq. 'SHF') then
    do j=0,nymin1
        do i=0,nxfield-1
            sshf(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
            if (zsec4(nxfield*(ny-j-1)+i+1).ne.0.) hflswitch=.true. ! Heat flux available
        enddo
    enddo
endif

!! SOLAR RADIATION
if(trim(fpname) .eq. 'SR') then
    do j=0,nymin1
        do i=0,nxfield-1
            ssr(i,j,1,n)=zsec4(nxfield*(ny-j-1)+i+1)
            if (ssr(i,j,1,n).lt.0.) ssr(i,j,1,n)=0.
        enddo
    enddo
endif

!! EW SURFACE STRESS
if(trim(fpname) .eq. 'EWSS') then
    do j=0,nymin1
        do i=0,nxfield-1
            ewss(i,j)=zsec4(nxfield*(ny-j-1)+i+1)
            if (zsec4(nxfield*(ny-j-1)+i+1).ne.0.) strswitch=.true. ! stress available
        enddo
    enddo
endif

!! NS SURFACE STRESS
if(trim(fpname) .eq. 'NSSS') then
    do j=0,nymin1
        do i=0,nxfield-1
            nsss(i,j)=zsec4(nxfield*(ny-j-1)+i+1)
            if (zsec4(nxfield*(ny-j-1)+i+1).ne.0.) strswitch=.true. ! stress available
        enddo
    enddo
endif

!! ECMWF OROGRAPHY
if(trim(fpname) .eq. 'ORO') then
    do j=0,nymin1
        do i=0,nxfield-1
            oro(i,j)=zsec4(nxfield*(ny-j-1)+i+1)/ga
        enddo
    enddo
endif

!! ECMWF OROGRAPHY
if(trim(fpname) .eq. 'EXCESSORO') then
    do j=0,nymin1
        do i=0,nxfield-1
            excessoro(i,j)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif

!! ECMWF LAND SEA MASK
if(trim(fpname) .eq. 'LSM') then
    do j=0,nymin1
        do i=0,nxfield-1
            lsm(i,j)=zsec4(nxfield*(ny-j-1)+i+1)
        enddo
    enddo
endif


  call grib_release(igrib)
  goto 10                      !! READ NEXT LEVEL OR PARAMETER
  !
  ! CLOSING OF INPUT DATA FILE
  !

50   call grib_close_file(ifile)

  !error message if no fields found with correct first longitude in it
  if (gotGrid.eq.0) then
    print*,'***ERROR: no fields found with correct first longitude'// &
         'messages'
    stop
  endif

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
    call shift_field(tth,nxfield,ny,nuvzmax,nuvz,2,n)
    call shift_field(qvh,nxfield,ny,nuvzmax,nuvz,2,n)
    call shift_field(uuh,nxfield,ny,nuvzmax,nuvz,1,1)
    call shift_field(vvh,nxfield,ny,nuvzmax,nuvz,1,1)
    call shift_field(wwh,nxfield,ny,nwzmax,nwz,1,1)
  endif

  do i=0,nxmin1
    do j=0,nymin1
      surfstr(i,j,1,n)=sqrt(ewss(i,j)**2+nsss(i,j)**2)
    end do
  end do

  if ((.not.hflswitch).or.(.not.strswitch)) then
    write(*,*) 'WARNING: No flux data contained in GRIB file ', &
         wfname(indj)

  ! CALCULATE USTAR AND SSHF USING THE PROFILE METHOD
  ! As ECMWF has increased the model resolution, such that now the first model
  ! level is at about 10 m (where 10-m wind is given), use the 2nd ECMWF level
  ! (3rd model level in FLEXPART) for the profile method
  !***************************************************************************

    do i=0,nxmin1
      do j=0,nymin1
        plev1=akz(3)+bkz(3)*ps(i,j,1,n)
        pmean=0.5*(ps(i,j,1,n)+plev1)
        tv=tth(i,j,3,n)*(1.+0.61*qvh(i,j,3,n))
        fu=-r_air*tv/ga/pmean
        hlev1=fu*(plev1-ps(i,j,1,n))   ! HEIGTH OF FIRST MODEL LAYER
        ff10m= sqrt(u10(i,j,1,n)**2+v10(i,j,1,n)**2)
        fflev1=sqrt(uuh(i,j,3)**2+vvh(i,j,3)**2)
        call pbl_profile(ps(i,j,1,n),td2(i,j,1,n),hlev1, &
             tt2(i,j,1,n),tth(i,j,3,n),ff10m,fflev1, &
             surfstr(i,j,1,n),sshf(i,j,1,n))
        if(sshf(i,j,1,n).gt.200.) sshf(i,j,1,n)=200.
        if(sshf(i,j,1,n).lt.-400.) sshf(i,j,1,n)=-400.
      end do
    end do
  endif


  ! Assign 10 m wind to model level at eta=1.0 to have one additional model
  ! level at the ground
  ! Specific humidity is taken the same as at one level above
  ! Temperature is taken as 2 m temperature
  !**************************************************************************

     do i=0,nxmin1
        do j=0,nymin1
           uuh(i,j,1)=u10(i,j,1,n)
           vvh(i,j,1)=v10(i,j,1,n)
           qvh(i,j,1,n)=qvh(i,j,2,n)
           tth(i,j,1,n)=tt2(i,j,1,n)
        end do
     end do

  if(iumax.ne.nuvz-1) stop 'READWIND: NUVZ NOT CONSISTENT'
  if(iwmax.ne.nwz)    stop 'READWIND: NWZ NOT CONSISTENT'

  return
888   write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
  write(*,*) ' #### ',wfname(indj),'                    #### '
  write(*,*) ' #### IS NOT GRIB FORMAT !!!                  #### '
  stop 'Execution terminated'
999   write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
  write(*,*) ' #### ',wfname(indj),'                    #### '
  write(*,*) ' #### CANNOT BE OPENED !!!                    #### '
  stop 'Execution terminated'

end subroutine readwind_ecmwf
