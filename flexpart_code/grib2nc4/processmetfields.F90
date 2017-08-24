subroutine processmetfields(ind,metdata_format,coordinates_provided, coordX, coordY)

  !                       i     o
  !*****************************************************************************
  !                                                                            *

  !  This subrotuine produces all the meteorological data in binary (fp)       *
  !  format. It includes all the subroutines that the traditional getfields    *
  !  would use to generate the meteorological data                             *
  !                                                                            *
  !  This routine is essential part of the grib2flexpart utility               *
  ! 
  !             D. Morton (Boreal Scienctific Computing)                       *
  !             D. Arnold (ZAMG)                                               *
  !     15 October 2015                                                        *
  !                                                                            *
  !*****************************************************************************
  !*****************************************************************************
 
  !  Changes Arnold, D. and Morton, D. (2015):                                 *
  !   -- description of local and common variables                             *
  !*****************************************************************************


  use par_mod
  use com_mod

  implicit none

  !****************************************************************************
  ! Subroutine Parameters:                                                    *
  !    input                                                                  *
  ! nstop                > 0, if trajectory has to be terminated              *
  !    output                                                                 *
  ! metdata_format       0 = unknown, 1 = ecmwf, 2 = gfs meteorological data  *
  !  
  integer :: ind, metdata_format, i, lastSlash
  integer :: dumpData
  character(len=512):: fpfname, dumpPath, filename
  integer :: coordX, coordY
  logical :: coordinates_provided


  !****************************************************************************

  !****************************************************************************
  ! Local variables                                                           *
  !
  ! indj                 indicates the number of the wind field to be read in *
  ! memaux               auxiliary variable to shuffle winds in memory        *
  ! indmin               remembers the number of wind fields already treated  *
  ! uu(0:nxmax,0:nymax,nuvzmax,2)  wind components in x-direction [m/s]       *
  ! vv(0:nxmax,0:nymax,nuvzmax,2)  wind components in y-direction [m/s]       *
  ! ww(0:nxmax,0:nymax,nwzmax,2)   wind components in z-direction [deltaeta/s]*
  ! tt(0:nxmax,0:nymax,nuvzmax,2)  temperature [K]                            *
  ! ps(0:nxmax,0:nymax,2)          surface pressure [Pa]                      *
  !
  integer :: memaux
  real :: uuh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: vvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: pvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: wwh(0:nxmax-1,0:nymax-1,nwzmax)
  real :: uuhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: vvhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: pvhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: wwhn(0:nxmaxn-1,0:nymaxn-1,nwzmax,maxnests)
  !****************************************************************************


  !****************************************************************************
  ! Global variables                                                          *
  !     from par_mod and com_mod                                              *
  ! nx,ny,nuvz,nwz       field dimensions in x,y and z direction              *
  ! indmin               remembers the number of wind fields already treated  *
  ! memind(2)            pointer, on which place the wind fields are stored   *
  ! memtime(2) [s]       times of the wind fields, which are kept in memory   *
  ! ndinterval [s]    time difference between the two wind fields read in     *
  ! ldirect                 1 for forward, -1 for backward simulation         * 
  ! numbwf                  actual number of wind fields                      *
  ! wftime(maxwf) [s]       times relative to beginning time of wind fields   *
  ! wfname(maxwf)           name of met file (used for performance timing out)*
  !****************************************************************************


#ifdef PERFTIMER 
  INTEGER millisecs_start, millisecs_stop, count_rate, count_max
#endif

!-----------------------------------------------------------------------------


  ! Current time is after 2nd wind field
  ! -> Resort wind field pointers, so that current time is between 1st and 2nd
  !***************************************************************************

    !memaux=memind(1)
    !memind(1)=memind(2)
    !memind(2)=memaux
    !memtime(1)=memtime(2)


  ! Read a new wind field and store it on place memind(2)
  !******************************************************
#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)
#endif
          if (metdata_format.eq.ecmwf_metdata) then
             call readwind_ecmwf(ind,memind(1),uuh,vvh,wwh)
             call readwind_nests(ind,memind(1),uuhn,vvhn,wwhn)
             call calcpar_ecmwf(memind(1),uuh,vvh,pvh) 
             call calcpar_nests(memind(1),uuhn,vvhn,pvhn,metdata_format)
             if ( coordinates_provided ) then
                 call verttransform_grib2nc4_ecmwf(memind(1),uuh,vvh,wwh,pvh,coordX,coordY)
             else
                 call verttransform_grib2nc4_ecmwf(memind(1),uuh,vvh,wwh,pvh,-1,-1)
             endif
             call verttransform_nests(memind(1),uuhn,vvhn,wwhn,pvhn)
             memtime(1)=wftime(ind)
          endif
          if (metdata_format.eq.gfs_metdata) then
             call readwind_gfs(ind,memind(1),uuh,vvh,wwh)
             call readwind_nests(ind,memind(1),uuhn,vvhn,wwhn)
             call calcpar_gfs(memind(1),uuh,vvh,pvh)
             call calcpar_nests(memind(1),uuhn,vvhn,pvhn,metdata_format)
             if ( coordinates_provided ) then
                 call verttransform_grib2nc4_gfs(memind(1),uuh,vvh,wwh,pvh,coordX,coordY)
             else
                 call verttransform_grib2nc4_gfs(memind(1),uuh,vvh,wwh,pvh,-1,-1)
             endif
             call verttransform_nests(memind(1),uuhn,vvhn,wwhn,pvhn)
             memtime(1)=wftime(ind)
          endif

#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
         PRINT *, 'Wall time to process: ', TRIM(wfname(ind)), &
                  ': ', (millisecs_stop-millisecs_start)/1000.0, ' seconds'
#endif


          !lastSlash = 0 
          !do i=1,len(wfname(ind))
          !  if (wfname(ind)(i:i).eq.'/') then
          !    lastSlash = i
          !  end if
          !end do
          filename = wfname(ind)(lastSlash+1:len(wfname(ind)))
       
          !if ( ldirect.eq.1 ) then
          !    fpfname = TRIM(filename) // '_fwd.fp'
          !else
          !    fpfname = TRIM(filename) // '_bwd.fp'
          !endif
          !print *, 'writing ',  TRIM(dumpPath) // '/' //  TRIM(fpfname)

#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)
#endif

          !CALL fpmetbinary_dump( TRIM(dumpPath) // '/' // TRIM(fpfname), memind(1))

#ifdef PERFTIMER
         !CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
         !PRINT *, 'Wall time to process: ', &
         !         ': ', (millisecs_stop-millisecs_start)/1000.0, ' seconds'
#endif

end subroutine processmetfields


