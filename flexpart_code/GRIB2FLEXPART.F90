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

program grib2flexpart

  !*****************************************************************************
  !                                                                            *
  !     This is the GRIB 2 FP conversion routine                               *
  !                                                                            *
  !     Author: M. Harustak                                                    *
  !                                                                            *
  !     05 October 2015                                                        *
  !                                                                            *
  !*****************************************************************************
  !                                                                            *
  ! Variables:                                                                 *
  !                                                                            *
  ! Constants:                                                                 *
  !                                                                            *
  !*****************************************************************************

  use point_mod
  use par_mod
  use com_mod
  use conv_mod

  implicit none

  integer :: metdata_format = unknown_metdata
  integer :: itime, nstop1
  integer :: i
  character(len=256) :: arg
  character(len=512) :: dumpPath
  character(len=512) :: inputFileName
  character(len=512) :: nestedFileName
  character(len=32) :: lsubgridTXT
  integer :: useAvailable = 0

  ! Print the GPL License statement
  !*******************************************************
#if defined CTBTO
  print*,'Welcome to GRIB2FLEXPART Version 1.0 CTBTO'
#else
  print*,'Welcome to GRIB2FLEXPART Version 1.0'
#endif

  print*,'FLEXPART is free software released under the GNU Genera'// &
       'l Public License.'

  ! no argument => error
  if (( iargc().lt.2 ).or.( iargc().eq.3)) then
    print *,' '
    print *,'Usage in use-available mode: grib2flexpart useAvailable <output directory>'
    print *,'Usage in command line mode: grib2flexpart forward|backward subgrid <output directory> <source file> [<source file>...]'
    print *,' '
    stop 'Error: Missing arguments'
  elseif ( iargc().eq.2 ) then
    call getarg(1,arg)
    if ( arg.eq."useAvailable" ) then
      print *,'Running in use-available mode'
      useAvailable = 1
    else
      print *,' '
      print *,'Usage in use-available mode: grib2flexpart useAvailable <output directory>'
      print *,'Usage in command line mode: grib2flexpart forward|backward subgrid <output directory> &
 <source file> [<source file>...]'
      print *,' '
      stop 'Error: Incorrect arguments'
    endif
  else
    ! 2 and more arguments => ok, parse arguments
    call getarg(1,arg)
    if ( arg.eq."forward") then
      ldirect = 1
    else if ( arg.eq."backward") then
      ldirect = -1
    else
      print *,' '
      print *,'Usage in use-available mode: grib2flexpart useAvailable <output directory>'
      print *,'Usage in command line mode: grib2flexpart forward|backward subgrid <output directory> &
<source file> [<source file>...]'
      print *,' '
      stop 'Error: Incorrect arguments'
    endif
    print *,'Running in command line mode'
    useAvailable = 0
  endif

  if ( useAvailable.eq.0 ) then
    call getarg(2,lsubgridTxt)
    read (lsubgridTxt, '(i10)') lsubgrid
    lsubgrid=1
    call getarg(3,dumpPath)
    do i=4,iargc()
      call getarg(i,inputFileName)
      numbwf = i - 4 + 1
      if ( ldirect.eq.1 ) then
        wfname(i-4+1) = inputFileName
      else
        wfname(iargc()+1-i) = inputFileName
      endif
    end do

  else
    call getarg(2,dumpPath)

    call readpaths

    call readcommand

    call readavailable
  endif

! Reset the times of the wind fields that are kept in memory to no time
 !**********************************************************************

  do i=1,2
    memind(i)=i
    memtime(i)=999999999
  end do

  ! Detect metdata format
  call detectformat(metdata_format)
  if (metdata_format.eq.ecmwf_metdata) then
    print*,'ECMWF metdata detected'
  elseif (metdata_format.eq.gfs_metdata) then
    print*,'NCEP metdata detected'
  else
    stop 'Unknown metdata format'
  endif

  ! Read the model grid specifications,
  ! both for the mother domain and eventual nests
  !**********************************************

  if (metdata_format.eq.ecmwf_metdata) call gridcheck_ecmwf
  if (metdata_format.eq.gfs_metdata) call gridcheck_gfs
  call gridcheck_nests

  do i=1,numbwf
    if ( ( useAvailable.eq.0 ).or.(numbwf.eq.1) ) then
      print *,' '
      print *,wfname(i)
      call convertfields( i, metdata_format, dumpPath)
    else
       if (((ldirect*wftime(i).le.0).and. &
             (ldirect*wftime(i+1).gt.0)).or. & 
          (ldirect*wftime(i).gt.0)) then
          print *,' '
          print *,wfname(i)
          call convertfields( i, metdata_format, dumpPath)
       endif
    endif
  end do

  write(*,*) 'CONGRATULATIONS: YOU HAVE SUCCESSFULLY COMPLETED A GRIB2FLE&
       &XPART PREPROCESSING RUN!'

end program grib2flexpart
