
subroutine getfpfields(itime,nstop,metdata_format)
  !                       i     o
  !*****************************************************************************
  !                                                                            *
  !  This subroutine manages the 3 data fields to be kept in memory.           *
  !  During the first time step of petterssen it has to be fulfilled that the  *
  !  first data field must have |wftime|<itime, i.e. the absolute value of     *
  !  wftime must be smaller than the absolute value of the current time in [s].*
  !  The other 2 fields are the next in time after the first one.              *
  !  Pointers (memind) are used, because otherwise one would have to resort the*
  !  wind fields, which costs a lot of computing time. Here only the pointers  *
  !  are resorted.                                                             *
  !                                                                            *
  !     Author: A. Stohl                                                       *
  !                                                                            *
  !     29 April 1994                                                          *
  !                                                                            *
  !*****************************************************************************
  !  Changes, Bernd C. Krueger, Feb. 2001:
  !   Variables tth,qvh,tthn,qvhn (on eta coordinates) in common block.
  !   Function of nstop extended.
  !*****************************************************************************
  !                                                                            *
  ! Variables:                                                                 *
  ! lwindinterval [s]    time difference between the two wind fields read in   *
  ! indj                 indicates the number of the wind field to be read in  *
  ! indmin               remembers the number of wind fields already treated   *
  ! memind(2)            pointer, on which place the wind fields are stored    *
  ! memtime(2) [s]       times of the wind fields, which are kept in memory    *
  ! itime [s]            current time since start date of trajectory calcu-    *
  !                      lation                                                *
  ! nstop                > 0, if trajectory has to be terminated               *
  ! nx,ny,nuvz,nwz       field dimensions in x,y and z direction               *
  ! uu(0:nxmax,0:nymax,nuvzmax,2)   wind components in x-direction [m/s]       *
  ! vv(0:nxmax,0:nymax,nuvzmax,2)   wind components in y-direction [m/s]       *
  ! ww(0:nxmax,0:nymax,nwzmax,2)    wind components in z-direction [deltaeta/s]*
  ! tt(0:nxmax,0:nymax,nuvzmax,2)   temperature [K]                            *
  ! ps(0:nxmax,0:nymax,2)           surface pressure [Pa]                      *
  !                                                                            *
  ! Constants:                                                                 *
  ! idiffmax             maximum allowable time difference between 2 wind      *
  !                      fields                                                *
  !                                                                            *
  !*****************************************************************************

  use par_mod
  use com_mod

  use fpmetbinary_mod

  implicit none

  integer :: indj,itime,nstop,memaux,metdata_format

  real :: uuh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: vvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: pvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: wwh(0:nxmax-1,0:nymax-1,nwzmax)
  real :: uuhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: vvhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: pvhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: wwhn(0:nxmaxn-1,0:nymaxn-1,nwzmax,maxnests)

  integer :: indmin = 1

  !character(len=120) :: fpdir   ! Directory the .fp files are in
  character(len=512) fpfname    ! .fp filename

#ifdef PERFTIMER 
  INTEGER millisecs_start, millisecs_stop, count_rate, count_max
#endif


  ! Check, if wind fields are available for the current time step
  !**************************************************************

  nstop=0

  if ((ldirect*wftime(1).gt.ldirect*itime).or. &
       (ldirect*wftime(numbwf).lt.ldirect*itime)) then
    write(*,*) 'FLEXPART WARNING: NO WIND FIELDS ARE AVAILABLE.'
    write(*,*) 'A TRAJECTORY HAS TO BE TERMINATED.'
    nstop=4
    return
  endif


  if ((ldirect*memtime(1).le.ldirect*itime).and. &
       (ldirect*memtime(2).gt.ldirect*itime)) then

  ! The right wind fields are already in memory -> don't do anything
  !*****************************************************************

    continue

  else if ((ldirect*memtime(2).le.ldirect*itime).and. &
       (memtime(2).ne.999999999)) then


  ! Current time is after 2nd wind field
  ! -> Resort wind field pointers, so that current time is between 1st and 2nd
  !***************************************************************************

    memaux=memind(1)
    memind(1)=memind(2)
    memind(2)=memaux
    memtime(1)=memtime(2)


  ! Read a new wind field and store it on place memind(2)
  !******************************************************

    do indj=indmin,numbwf-1
       if (ldirect*wftime(indj+1).gt.ldirect*itime) then

#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)
#endif
           ! Read in a single .fp file, placing contents at index 2
           if ( ldirect.eq.1 ) then 
             fpfname = TRIM(path(3)) // TRIM(wfname(indj+1)) // '_fwd.fp'
           else
             fpfname = TRIM(path(3)) // TRIM(wfname(indj+1)) // '_bwd.fp'
           endif
           print *, 'loading... ',  TRIM(fpfname)
           CALL fpmetbinary_load(TRIM(fpfname), memind(2)) 
           memtime(2)=wftime(indj+1)
           nstop = 1

#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
         PRINT *, 'Wall time to process: ', TRIM(fpfname), &
                  ': ', (millisecs_stop-millisecs_start)/1000.0, ' seconds'
#endif


           goto 40

       endif
    end do
 40   indmin=indj

  else

  ! No wind fields, which can be used, are currently in memory
  ! -> read both wind fields
  !***********************************************************

     do indj=indmin,numbwf-1
        if ((ldirect*wftime(indj).le.ldirect*itime).and. &
             (ldirect*wftime(indj+1).gt.ldirect*itime)) then
           memind(1)=1

#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)
#endif
            ! Read in first .fp file
            if ( ldirect.eq.1 ) then
              fpfname = TRIM(path(3)) // TRIM(wfname(indj)) // '_fwd.fp'
            else
              fpfname = TRIM(path(3)) // TRIM(wfname(indj)) // '_bwd.fp'
            endif
            print *, 'loading... ',  TRIM(fpfname)
            CALL fpmetbinary_load(TRIM(fpfname), memind(1)) 
            memtime(1)=wftime(indj)
            memind(2)=2
#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
         PRINT *, 'Wall time to process: ', TRIM(fpfname), &
                  ': ', (millisecs_stop-millisecs_start)/1000.0, ' seconds'
#endif

#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)
#endif
            ! Read in second .fp file
            if ( ldirect.eq.1 ) then
              fpfname = TRIM(path(3)) // TRIM(wfname(indj+1)) // '_fwd.fp'
            else
              fpfname = TRIM(path(3)) // TRIM(wfname(indj+1)) // '_bwd.fp'
            endif
            print *, 'loading... ',  TRIM(fpfname)
            CALL fpmetbinary_load(TRIM(fpfname), memind(2)) 
            memtime(2)=wftime(indj+1)
            nstop = 1
#ifdef PERFTIMER
         CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
         PRINT *, 'Wall time to process: ', TRIM(fpfname), &
                  ': ', (millisecs_stop-millisecs_start)/1000.0, ' seconds'
#endif


            goto 60
        endif
     end do
 60   indmin=indj

  endif

  lwindinterv=abs(memtime(2)-memtime(1))

  if (lwindinterv.gt.idiffmax) nstop=3

end subroutine getfpfields


