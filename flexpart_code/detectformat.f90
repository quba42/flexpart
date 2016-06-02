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

subroutine detectformat(metdata_format)

  !*****************************************************************************
  !                                                                            *
  !   This routine reads the 1st file with windfields to determine             *
  !   the format.                                                              *
  !                                                                            *
  !     Authors: M. Harustak                                                   *
  !                                                                            *
  !     6 May 2015                                                             *
  !                                                                            *
  !*****************************************************************************
  !                                                                            *
  ! Variables:                                                                 *
  ! fname                file name of file to check                            *
  !                                                                            *
  !*****************************************************************************

  use par_mod
  use com_mod
  use grib_api


  implicit none

  character(len=255) :: filename
  character(len=255) :: wfname1(maxwf)
  integer :: metdata_format
  integer :: gfileid, status, gribid, centrenum

  if ( maxwf.le.0 ) then
    print*,'No wind file available'
    metdata_format = unknown_metdata
    return
  endif

  filename = path(3)(1:length(3)) // trim(wfname(1))
  
  ! Try to open the grib file - if successful, 
  ! gfileid will be the file handle
  call grib_open_file(gfileid, TRIM(filename), 'r', status)
  if (status.ne. GRIB_SUCCESS) then 
    print *, 'Unable to open: ', filename
    metdata_format = unknown_metdata
    return
  endif

  ! Get the GRIB id for the first message in the GRIB file.  This message
  ! (and all the others) should have the "centre" field in it.  "gribid"
  ! will be the handle for this message
  call grib_new_from_file(gfileid, gribid, status)

  ! Now, from the "gribid" message, get the value of the "centre" field
  call grib_get(gribid, 'centre', centrenum)

  if (centrenum .EQ. 7) then
    metdata_format = gfs_metdata
  elseif (centrenum == 98) then
    metdata_format = ecmwf_metdata
  else
    metdata_format = unknown_metdata
  endif

  call grib_close_file(gfileid)


end subroutine detectformat
