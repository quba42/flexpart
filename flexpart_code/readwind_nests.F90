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

subroutine readwind_nests(indj,n,uuhn,vvhn,wwhn)
  !                           i   i  o    o    o
  !*****************************************************************************
  !                                                                            *
  !     This routine reads the wind fields for the nested model domains.       *
  !     It is similar to subroutine readwind, which reads the mother domain.   *
  !                                                                            *
  !     Authors: A. Stohl, G. Wotawa                                           *
  !                                                                            *
  !     8 February 1999                                                        *
  !                                                                            *
  !     Last update: 17 October 2000, A. Stohl                                 *
  !                                                                            *
  !*****************************************************************************
  !  Changes, Bernd C. Krueger, Feb. 2001:                                     *
  !        Variables tthn and qvhn (on eta coordinates) in common block        *
  !  CHANGE: 11/01/2008, Harald Sodemann, GRIB1/2 input with ECMWF grib_api    *
  !  CHANGE: 03/12/2008, Harald Sodemann, update to f90 with ECMWF grib_api    *
  !*****************************************************************************
  !  Changes Arnold, D. and Morton, D. (2015):                                 *
  !   -- description of local and common variables                             *
  !*****************************************************************************

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
  ! uuhn,vvhn, wwhn     wind components for the input nest in ECMWF      *
  !                     model levels                                     *  
  integer :: indj,n
  real :: uuhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: vvhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: wwhn(0:nxmaxn-1,0:nymaxn-1,nwzmax,maxnests)
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
  integer :: isec1(56),isec2(22+nxmaxn+nymaxn)
  real(kind=4) :: zsec4(jpunp)
  real(kind=8) :: xauxin,yauxin
  real(kind=4) :: xaux,yaux,xaux0,yaux0
  real :: ewss(0:nxmaxn-1,0:nymaxn-1),nsss(0:nxmaxn-1,0:nymaxn-1)
  real :: plev1,pmean,tv,fu,hlev1,ff10m,fflev1
  logical :: hflswitch,strswitch
  !
  ! Other variables:
  ! i,j,k               loop control indices in each direction           *
  ! levdiff2            number of model levels - number of model levels  *
  !                     for the staggered vertical wind +1               *                                    
  ! ifield              index to control field read in                   *
  ! iumax
  ! iwmax
  integer ::i,j,k,levdiff2,ifield,iumax,iwmax,l
  !***********************************************************************

  !***********************************************************************
  ! Local constants                                                      *
  !HSO  grib api error messages:
  character(len=24) :: gribErrorMsg = 'Error reading grib file'
  character(len=20) :: gribFunction = 'readwind_nests'
  !***********************************************************************
  !***********************************************************************
  ! Global variables                                                     *
  !     from par_mod and com_mod                                         *
  ! wfname             File name of data to be read in                   *
  ! maxnests           maximum number of nests                           *
  ! nxn,nyn,nuvz,nwz   expected field dimensions                         *
  ! nxmaxn,nymaxn           maximum dimension of nested wind fields in   *
  !                         x and y direction, respectively              *
  ! nuvzmax,nwzmax     maximum dimension of (u,v) and (w) wind fields in z
  !                    direction (for fields on eta levels)              *
  ! nlev_ec            number of vertical levels ecmwf model             *
  ! u10n,v10n          wind fields at 10 m                               *
  ! tt2n,td2n          temperature and dew point temperature at 2 m      *
  ! tthn,qvhn          temperature and specific humidity on original     *
  !                    eta levels                                        *
  ! psn                surface pressure                                  *
  ! sdn                snow depth (but not used afterwards!)             *
  ! msln               mean sea level pressure                           *
  ! ttcn               total cloud cover                                 *
  ! lsprecn            large scale precipitation                         *
  ! cprecn             convective precipitation                          *
  ! sshfn              sensible heat flux                                *
  ! ssrn               solar radiation                                   *
  ! surfstrn           surface stress                                  *
  ! oron               orography                                         *
  ! excessoron         standard deviation of orography                   *
  ! lsmn               land sea mask                                     *
  ! r_air              individual gas constant for dry air [J/kg/K]      *
  ! ga                 gravity acceleration of earth [m/s**2]            *
  !***********************************************************************

!-----------------------------------------------------------------------------

  do l=1,numbnests
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
    print *, 'readwind_nests(): Loading Vtable: ', vtable_path
    call vtable_load_by_name(vtable_path, my_vtable)
    print *, 'readwind_nests(): Vtable Initialized: ', my_vtable%initialized
    print *, 'readwind_nests(): Vtable num_entries: ', my_vtable%num_entries
    !!!!!!!!!!!!!!!!!!!  VTABLE code
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





    ifile=0
    igrib=0
    iret=0

  !
  ! OPENING OF DATA FILE (GRIB CODE)
  !

5   call grib_open_file(ifile,path(numpath+2*(l-1)+1) &
         (1:length(numpath+2*(l-1)+1))//trim(wfnamen(l,indj)),'r')
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
    print *, 'readwind_nests(): fpname: ', trim(fpname)


    !!!!!!!!!!!!!!!!!!!  VTABLE code
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






  !first see if we read GRIB1 or GRIB2
  call grib_get_int(igrib,'editionNumber',gribVer,iret)
  call grib_check(iret,gribFunction,gribErrorMsg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  WARNING  
!!!!! DJM 2016-01-19 - WARNING - we might want to put in the call to 
!!!!! grib2check(), just as it is in the mother grid

    call grib_get_int(igrib,'level', current_grib_level,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)





  !HSO  get the size and data of the values array
  if (isec1(6).ne.-1) then
    call grib_get_real4_array(igrib,'values',zsec4,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  WARNING  
!!!!! DJM 2016-01-19 - WARNING - we might want to put in the zsec4 
!!!!! conversion, just as it is in the mother grid    
  endif

  !HSO  get the required fields from section 2 in a gribex compatible manner
  if(ifield.eq.1) then
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
  if(isec2(2).ne.nxn(l)) stop &
       'READWIND: NX NOT CONSISTENT FOR A NESTING LEVEL'
  if(isec2(3).ne.nyn(l)) stop &
       'READWIND: NY NOT CONSISTENT FOR A NESTING LEVEL'
  if(isec2(12)/2-1.ne.nlev_ec) stop 'READWIND: VERTICAL DISCRET&
       &IZATION NOT CONSISTENT FOR A NESTING LEVEL'
  endif ! ifield

  !HSO  get the second part of the grid dimensions only from GRiB1 messages

!!! DJM 2016-05-29 -- this check for gribVer doesn't make sense, and causes
!!! failure with pure GRIB2, so I'm commenting out
!if ((gribVer.eq.1).and.(gotGrid.eq.0)) then
if (gotGrid.eq.0) then
    call grib_get_real8(igrib,'longitudeOfFirstGridPointInDegrees', &
         xauxin,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_real8(igrib,'latitudeOfLastGridPointInDegrees', &
         yauxin,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    xaux=xauxin
    yaux=yauxin
    xaux0=xlon0n(l)
    yaux0=ylat0n(l)
    if(xaux.lt.0.) xaux=xaux+360.
    if(yaux.lt.0.) yaux=yaux+360.
    if(xaux0.lt.0.) xaux0=xaux0+360.
    if(yaux0.lt.0.) yaux0=yaux0+360.
    if(xaux.ne.xaux0) &
         stop 'READWIND: LOWER LEFT LONGITUDE NOT CONSISTENT FOR A NES&
         &TING LEVEL'
    if(yaux.ne.yaux0) &
         stop 'READWIND: LOWER LEFT LATITUDE NOT CONSISTENT FOR A NEST&
         &ING LEVEL'
    gotGrid=1
  endif

k = current_grib_level

!! TEMPERATURE
if(trim(fpname) .eq. 'TT') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            tthn(i,j,nlev_ec-k+2,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! U VELOCITY
if(trim(fpname) .eq. 'UU') then
    iumax=max(iumax,nlev_ec-k+1)
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            uuhn(i,j,nlev_ec-k+2,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! V VELOCITY
if(trim(fpname) .eq. 'VV') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            vvhn(i,j,nlev_ec-k+2,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! W VELOCITY
if(trim(fpname) .eq. 'ETADOT') then
    iwmax=max(iwmax,nlev_ec-k+1)
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            wwhn(i,j,nlev_ec-k+1,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! SPEC. HUMIDITY
if(trim(fpname) .eq. 'QV') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            qvhn(i,j,nlev_ec-k+2,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
            if (qvhn(i,j,nlev_ec-k+2,n,l) .lt. 0.) &
                qvhn(i,j,nlev_ec-k+2,n,l) = 0.
  !         this is necessary because the gridded data may contain
  !         spurious negative values
        enddo
    enddo
endif


!! SURF. PRESS
if(trim(fpname) .eq. 'PS') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            psn(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! SNOW DEPTH
if(trim(fpname) .eq. 'SD') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            sdn(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! SEA LEVEL PRESS.
if(trim(fpname) .eq. 'MSL') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            msln(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! CLOUD COVER
if(trim(fpname) .eq. 'TCC') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            tccn(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! 10 M U VELOCITY
if(trim(fpname) .eq. 'U10') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            u10n(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! 10 M V VELOCITY
if(trim(fpname) .eq. 'V10') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            v10n(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! 2 M TEMPERATURE
if(trim(fpname) .eq. 'T2') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            tt2n(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! 2 M DEW POINT
if(trim(fpname) .eq. 'TD2') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            td2n(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! LARGE SCALE PREC.
if(trim(fpname) .eq. 'LSPREC') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            lsprecn(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
            if (lsprecn(i,j,1,n,l).lt.0.) lsprecn(i,j,1,n,l)=0.
        enddo
    enddo
endif

!! CONVECTIVE PREC.
if(trim(fpname) .eq. 'CONVPREC') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            convprecn(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
            if (convprecn(i,j,1,n,l).lt.0.) convprecn(i,j,1,n,l)=0.
        enddo
    enddo
endif

!! SENS. HEAT FLUX
if(trim(fpname) .eq. 'SHF') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            sshfn(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
            if (zsec4(nxn(l)*(nyn(l)-j-1)+i+1).ne.0.) hflswitch=.true.    ! Heat flux available
        enddo
    enddo
endif

!! SOLAR RADIATION
if(trim(fpname) .eq. 'SR') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            ssrn(i,j,1,n,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
            if (ssrn(i,j,1,n,l).lt.0.) ssrn(i,j,1,n,l)=0.
        enddo
    enddo
endif

!! EW SURFACE STRESS
if(trim(fpname) .eq. 'EWSS') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            ewss(i,j) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
            if (zsec4(nxn(l)*(nyn(l)-j-1)+i+1).ne.0.) strswitch=.true.    ! stress available
        enddo
    enddo
endif

!! NS SURFACE STRESS
if(trim(fpname) .eq. 'NSSS') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            nsss(i,j) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
            if (zsec4(nxn(l)*(nyn(l)-j-1)+i+1).ne.0.) strswitch=.true.    ! stress available
        enddo
    enddo
endif

!! ECMWF OROGRAPHY
if(trim(fpname) .eq. 'ORO') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            oron(i,j,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)/ga
        enddo
    enddo
endif

!! STANDARD DEVIATION OF OROGRAPHY
if(trim(fpname) .eq. 'EXCESSORO') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            excessoron(i,j,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
        enddo
    enddo
endif

!! ECMWF LAND SEA MASK
if(trim(fpname) .eq. 'LSM') then
    do j=0,nyn(1)-1
        do i=0,nxn(l)-1
            lsmn(i,j,l) = zsec4(nxn(l)*(nyn(l)-j-1)+i+1)
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
    print*,'***ERROR: input file needs to contain GRiB1 formatted'// &
         'messages'
    stop
  endif

  if(levdiff2.eq.0) then
    iwmax=nlev_ec+1
    do i=0,nxn(l)-1
      do j=0,nyn(l)-1
        wwhn(i,j,nlev_ec+1,l)=0.
      end do
    end do
  endif

  do i=0,nxn(l)-1
    do j=0,nyn(l)-1
      surfstrn(i,j,1,n,l)=sqrt(ewss(i,j)**2+nsss(i,j)**2)
    end do
  end do

  if ((.not.hflswitch).or.(.not.strswitch)) then
    write(*,*) 'WARNING: No flux data contained in GRIB file ', &
         wfnamen(l,indj)

  ! CALCULATE USTAR AND SSHF USING THE PROFILE METHOD
  ! As ECMWF has increased the model resolution, such that now the first model
  ! level is at about 10 m (where 10-m wind is given), use the 2nd ECMWF level
  ! (3rd model level in FLEXPART) for the profile method
  !***************************************************************************

    do i=0,nxn(l)-1
      do j=0,nyn(l)-1
        plev1=akz(3)+bkz(3)*psn(i,j,1,n,l)
        pmean=0.5*(psn(i,j,1,n,l)+plev1)
        tv=tthn(i,j,3,n,l)*(1.+0.61*qvhn(i,j,3,n,l))
        fu=-r_air*tv/ga/pmean
        hlev1=fu*(plev1-psn(i,j,1,n,l))   ! HEIGTH OF FIRST MODEL LAYER
        ff10m= sqrt(u10n(i,j,1,n,l)**2+v10n(i,j,1,n,l)**2)
        fflev1=sqrt(uuhn(i,j,3,l)**2+vvhn(i,j,3,l)**2)
        call pbl_profile(psn(i,j,1,n,l),td2n(i,j,1,n,l),hlev1, &
             tt2n(i,j,1,n,l),tthn(i,j,3,n,l),ff10m,fflev1, &
             surfstrn(i,j,1,n,l),sshfn(i,j,1,n,l))
        if(sshfn(i,j,1,n,l).gt.200.) sshfn(i,j,1,n,l)=200.
        if(sshfn(i,j,1,n,l).lt.-400.) sshfn(i,j,1,n,l)=-400.
      end do
    end do
  endif


  ! Assign 10 m wind to model level at eta=1.0 to have one additional model
  ! level at the ground
  ! Specific humidity is taken the same as at one level above
  ! Temperature is taken as 2 m temperature
  !**************************************************************************

    do i=0,nxn(l)-1
      do j=0,nyn(l)-1
        uuhn(i,j,1,l)=u10n(i,j,1,n,l)
        vvhn(i,j,1,l)=v10n(i,j,1,n,l)
        qvhn(i,j,1,n,l)=qvhn(i,j,2,n,l)
        tthn(i,j,1,n,l)=tt2n(i,j,1,n,l)
      end do
    end do

    if(iumax.ne.nuvz-1) stop &
         'READWIND: NUVZ NOT CONSISTENT FOR A NESTING LEVEL'
    if(iwmax.ne.nwz) stop &
         'READWIND: NWZ NOT CONSISTENT FOR A NESTING LEVEL'

  end do

  return
888   write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
  write(*,*) ' #### ',wfnamen(l,indj),' FOR NESTING LEVEL  #### '
  write(*,*) ' #### ',l,' IS NOT GRIB FORMAT !!!           #### '
  stop 'Execution terminated'


999   write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
  write(*,*) ' #### ',wfnamen(l,indj),'                    #### '
  write(*,*) ' #### CANNOT BE OPENED FOR NESTING LEVEL ',l,'####'

end subroutine readwind_nests
